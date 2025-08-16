//! Code to allow printing out notes to a [cat printer](
//! https://korekawaii.com/products/kawaii-portable-cat-thermal-printer).
//!
//! Thanks to <https://github.com/WerWolv/PythonCatPrinter> and
//! <https://github.com/NaitLee/Cat-Printer> for reverse-engineering
//! the protocol!

use std::time::Duration;

use btleplug::api::{
    Central, CharPropFlags, Characteristic, Manager as _, Peripheral as _, ScanFilter,
};
use btleplug::platform::{Adapter, Manager, Peripheral};
use uuid::uuid;

use crate::{Note, fonts};

/// CRC8 table extracted from APK, pretty standard though
const CRC8_TABLE: [u8; 256] = [
    0x00, 0x07, 0x0e, 0x09, 0x1c, 0x1b, 0x12, 0x15, 0x38, 0x3f, 0x36, 0x31, 0x24, 0x23, 0x2a, 0x2d,
    0x70, 0x77, 0x7e, 0x79, 0x6c, 0x6b, 0x62, 0x65, 0x48, 0x4f, 0x46, 0x41, 0x54, 0x53, 0x5a, 0x5d,
    0xe0, 0xe7, 0xee, 0xe9, 0xfc, 0xfb, 0xf2, 0xf5, 0xd8, 0xdf, 0xd6, 0xd1, 0xc4, 0xc3, 0xca, 0xcd,
    0x90, 0x97, 0x9e, 0x99, 0x8c, 0x8b, 0x82, 0x85, 0xa8, 0xaf, 0xa6, 0xa1, 0xb4, 0xb3, 0xba, 0xbd,
    0xc7, 0xc0, 0xc9, 0xce, 0xdb, 0xdc, 0xd5, 0xd2, 0xff, 0xf8, 0xf1, 0xf6, 0xe3, 0xe4, 0xed, 0xea,
    0xb7, 0xb0, 0xb9, 0xbe, 0xab, 0xac, 0xa5, 0xa2, 0x8f, 0x88, 0x81, 0x86, 0x93, 0x94, 0x9d, 0x9a,
    0x27, 0x20, 0x29, 0x2e, 0x3b, 0x3c, 0x35, 0x32, 0x1f, 0x18, 0x11, 0x16, 0x03, 0x04, 0x0d, 0x0a,
    0x57, 0x50, 0x59, 0x5e, 0x4b, 0x4c, 0x45, 0x42, 0x6f, 0x68, 0x61, 0x66, 0x73, 0x74, 0x7d, 0x7a,
    0x89, 0x8e, 0x87, 0x80, 0x95, 0x92, 0x9b, 0x9c, 0xb1, 0xb6, 0xbf, 0xb8, 0xad, 0xaa, 0xa3, 0xa4,
    0xf9, 0xfe, 0xf7, 0xf0, 0xe5, 0xe2, 0xeb, 0xec, 0xc1, 0xc6, 0xcf, 0xc8, 0xdd, 0xda, 0xd3, 0xd4,
    0x69, 0x6e, 0x67, 0x60, 0x75, 0x72, 0x7b, 0x7c, 0x51, 0x56, 0x5f, 0x58, 0x4d, 0x4a, 0x43, 0x44,
    0x19, 0x1e, 0x17, 0x10, 0x05, 0x02, 0x0b, 0x0c, 0x21, 0x26, 0x2f, 0x28, 0x3d, 0x3a, 0x33, 0x34,
    0x4e, 0x49, 0x40, 0x47, 0x52, 0x55, 0x5c, 0x5b, 0x76, 0x71, 0x78, 0x7f, 0x6a, 0x6d, 0x64, 0x63,
    0x3e, 0x39, 0x30, 0x37, 0x22, 0x25, 0x2c, 0x2b, 0x06, 0x01, 0x08, 0x0f, 0x1a, 0x1d, 0x14, 0x13,
    0xae, 0xa9, 0xa0, 0xa7, 0xb2, 0xb5, 0xbc, 0xbb, 0x96, 0x91, 0x98, 0x9f, 0x8a, 0x8d, 0x84, 0x83,
    0xde, 0xd9, 0xd0, 0xd7, 0xc2, 0xc5, 0xcc, 0xcb, 0xe6, 0xe1, 0xe8, 0xef, 0xfa, 0xfd, 0xf4, 0xf3,
];

/// Using [`CRC8_TABLE`], get the CRC8 value of the provided data.
fn crc8(data: impl AsRef<[u8]>) -> u8 {
    let mut crc = 0;
    for byte in data.as_ref() {
        crc = CRC8_TABLE[(crc ^ byte) as usize];
    }
    crc
}

/// Convert a (command, data) pair into a message.
///
/// # Panics
///
/// If `data.len()` is over 255 (`u8::MAX`), this function will panic.
fn get_message(command: u8, data: Vec<u8>) -> Vec<u8> {
    assert!(
        u8::try_from(data.len()).is_ok(),
        "data provided to get_message too large"
    );

    let crc = crc8(&data);

    #[expect(
        clippy::cast_possible_truncation,
        reason = "false positive - verified otherwise with assert"
    )]
    [
        vec![0x51, 0x78],
        vec![command, 0x00, data.len() as u8, 0x00],
        data,
        vec![crc, 0xFF],
    ]
    .concat()
}

/// The direction to move the paper for [`move_paper`].
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum MovePaperDir {
    /// Pull backwards.
    #[allow(dead_code, reason = "well this still exists")]
    Retract,
    /// Push forwards.
    #[allow(dead_code, reason = "well this still exists")]
    Feed,
}

/// Create a Vec<u8> for moving the paper in `dir` for `px` pixels.
pub fn move_paper(dir: MovePaperDir, px: u16) -> Vec<u8> {
    get_message(
        match dir {
            MovePaperDir::Retract => 0xA0,
            MovePaperDir::Feed => 0xA1,
        },
        px.to_be_bytes().to_vec(),
    )
}

const MIRROR_TABLE: [u8; 256] = [
    0x00, 0x80, 0x40, 0xC0, 0x20, 0xA0, 0x60, 0xE0, 0x10, 0x90, 0x50, 0xD0, 0x30, 0xB0, 0x70, 0xF0,
    0x08, 0x88, 0x48, 0xC8, 0x28, 0xA8, 0x68, 0xE8, 0x18, 0x98, 0x58, 0xD8, 0x38, 0xB8, 0x78, 0xF8,
    0x04, 0x84, 0x44, 0xC4, 0x24, 0xA4, 0x64, 0xE4, 0x14, 0x94, 0x54, 0xD4, 0x34, 0xB4, 0x74, 0xF4,
    0x0C, 0x8C, 0x4C, 0xCC, 0x2C, 0xAC, 0x6C, 0xEC, 0x1C, 0x9C, 0x5C, 0xDC, 0x3C, 0xBC, 0x7C, 0xFC,
    0x02, 0x82, 0x42, 0xC2, 0x22, 0xA2, 0x62, 0xE2, 0x12, 0x92, 0x52, 0xD2, 0x32, 0xB2, 0x72, 0xF2,
    0x0A, 0x8A, 0x4A, 0xCA, 0x2A, 0xAA, 0x6A, 0xEA, 0x1A, 0x9A, 0x5A, 0xDA, 0x3A, 0xBA, 0x7A, 0xFA,
    0x06, 0x86, 0x46, 0xC6, 0x26, 0xA6, 0x66, 0xE6, 0x16, 0x96, 0x56, 0xD6, 0x36, 0xB6, 0x76, 0xF6,
    0x0E, 0x8E, 0x4E, 0xCE, 0x2E, 0xAE, 0x6E, 0xEE, 0x1E, 0x9E, 0x5E, 0xDE, 0x3E, 0xBE, 0x7E, 0xFE,
    0x01, 0x81, 0x41, 0xC1, 0x21, 0xA1, 0x61, 0xE1, 0x11, 0x91, 0x51, 0xD1, 0x31, 0xB1, 0x71, 0xF1,
    0x09, 0x89, 0x49, 0xC9, 0x29, 0xA9, 0x69, 0xE9, 0x19, 0x99, 0x59, 0xD9, 0x39, 0xB9, 0x79, 0xF9,
    0x05, 0x85, 0x45, 0xC5, 0x25, 0xA5, 0x65, 0xE5, 0x15, 0x95, 0x55, 0xD5, 0x35, 0xB5, 0x75, 0xF5,
    0x0D, 0x8D, 0x4D, 0xCD, 0x2D, 0xAD, 0x6D, 0xED, 0x1D, 0x9D, 0x5D, 0xDD, 0x3D, 0xBD, 0x7D, 0xFD,
    0x03, 0x83, 0x43, 0xC3, 0x23, 0xA3, 0x63, 0xE3, 0x13, 0x93, 0x53, 0xD3, 0x33, 0xB3, 0x73, 0xF3,
    0x0B, 0x8B, 0x4B, 0xCB, 0x2B, 0xAB, 0x6B, 0xEB, 0x1B, 0x9B, 0x5B, 0xDB, 0x3B, 0xBB, 0x7B, 0xFB,
    0x07, 0x87, 0x47, 0xC7, 0x27, 0xA7, 0x67, 0xE7, 0x17, 0x97, 0x57, 0xD7, 0x37, 0xB7, 0x77, 0xF7,
    0x0F, 0x8F, 0x4F, 0xCF, 0x2F, 0xAF, 0x6F, 0xEF, 0x1F, 0x9F, 0x5F, 0xDF, 0x3F, 0xBF, 0x7F, 0xFF,
];

/// 384 pixels / 8 per byte = 48 bytes
pub fn draw_line(line: [u8; 48]) -> Vec<u8> {
    let mut line_out = Vec::new();
    for byte in line {
        line_out.push(MIRROR_TABLE[byte as usize]);
    }
    get_message(0xA2, line_out)
}

/// Begin printing.
pub fn begin_lattice() -> Vec<u8> {
    get_message(
        0xa6,
        vec![
            0xaa, 0x55, 0x17, 0x38, 0x44, 0x5f, 0x5f, 0x5f, 0x44, 0x38, 0x2c,
        ],
    )
}

/// Unknown purpose - done at the start of printing
pub fn set_dpi_as_200() -> Vec<u8> {
    get_message(0xa4, 50u16.to_be_bytes().to_vec())
}

/// Unknown purpose - done at the start of printing
/// seems it could refresh device state & apply config
pub fn get_device_state() -> Vec<u8> {
    get_message(0xa3, 0u16.to_be_bytes().to_vec())
}

/// Unknown purpose - done at the start of printing
/// seems it could refresh device state & apply config
pub fn update_device() -> Vec<u8> {
    get_message(0xa9, 0u16.to_be_bytes().to_vec())
}

/// Begin printing on older models.
pub fn start_printing_old() -> Vec<u8> {
    vec![0x51, 0x78, 0xa3, 0x00, 0x01, 0x00, 0x00, 0x00, 0xff]
}

/// Begin printing on newer models.
pub fn start_printing_new() -> Vec<u8> {
    vec![0x12, 0x51, 0x78, 0xa3, 0x00, 0x01, 0x00, 0x00, 0x00, 0xff]
}

/// Apply the previously set energy to the printer.
pub fn apply_energy() -> Vec<u8> {
    get_message(0xbe, 0x01u16.to_be_bytes().to_vec())
}

/// Set how quick to move paper. Lower is faster?
pub fn set_speed(speed: u16) -> Vec<u8> {
    get_message(0xbd, speed.to_be_bytes().to_vec())
}

/// End printing.
pub fn end_lattice() -> Vec<u8> {
    get_message(
        0xa6,
        vec![
            0xaa, 0x55, 0x17, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x17,
        ],
    )
}

/// Draw a 384 wide image. 384 pixels / 8 per byte = 48 bytes
pub async fn draw_image(
    image: Vec<[u8; 48]>,
    device: &Peripheral,
    ch: &Characteristic,
) -> btleplug::Result<()> {
    device
        .write(
            ch,
            &get_device_state(),
            btleplug::api::WriteType::WithResponse,
        )
        .await?;

    if let Some(props) = device.properties().await?
        && ["GB03"].contains(&props.local_name.unwrap_or("UNKNOWN".to_string()).as_str())
    {
        device
            .write(
                ch,
                &start_printing_new(),
                btleplug::api::WriteType::WithResponse,
            )
            .await?;
    } else {
        device
            .write(
                ch,
                &start_printing_old(),
                btleplug::api::WriteType::WithResponse,
            )
            .await?;
    }

    device
        .write(
            ch,
            &set_dpi_as_200(),
            btleplug::api::WriteType::WithResponse,
        )
        .await?;

    set_meta(device, ch).await?;

    device
        .write(ch, &apply_energy(), btleplug::api::WriteType::WithResponse)
        .await?;

    device
        .write(ch, &update_device(), btleplug::api::WriteType::WithResponse)
        .await?;

    device
        .write(ch, &begin_lattice(), btleplug::api::WriteType::WithResponse)
        .await?;

    for line in image {
        device
            .write(ch, &draw_line(line), btleplug::api::WriteType::WithResponse)
            .await?;
        device
            .write(
                ch,
                &move_paper(MovePaperDir::Feed, 1),
                btleplug::api::WriteType::WithResponse,
            )
            .await?;

        // prevent jamming:
        smol::Timer::after(Duration::from_millis(40)).await;
    }

    device
        .write(ch, &end_lattice(), btleplug::api::WriteType::WithResponse)
        .await?;

    device
        .write(ch, &set_speed(8), btleplug::api::WriteType::WithResponse)
        .await?;

    device
        .write(
            ch,
            &move_paper(MovePaperDir::Feed, 500),
            btleplug::api::WriteType::WithResponse,
        )
        .await?;

    device
        .write(
            ch,
            &get_device_state(),
            btleplug::api::WriteType::WithResponse,
        )
        .await?;

    Ok(())
}

/// Create a byte from a `[bool; 8]`.
fn byte_from_bits(bits: [bool; 8]) -> u8 {
    let mut out = 0u8;
    for (i, val) in bits.iter().enumerate() {
        if *val {
            out |= 1 << i;
        }
    }
    out
}
/// Convert a grayscale image into a BW image of bools.
pub fn grayscale_image_to_bw_image(image: impl AsRef<[Vec<u8>]>) -> Vec<[u8; 48]> {
    image
        .as_ref()
        .iter()
        .map(|line| {
            let mut out_line = Vec::with_capacity(384);
            for bit in line {
                assert!(
                    out_line.len() < 383,
                    "provided image to grayscale_image_to_bw_image too wide"
                );
                out_line.push(*bit >= u8::MAX / 2);
            }
            let mut out = Vec::with_capacity(48);
            for ele in out_line.chunks(8) {
                out.push(byte_from_bits(ele.try_into().unwrap()));
            }
            out.try_into().unwrap()
        })
        .collect()
}

/// If a characteristic exists for the provided device that has the correct UUID and ops,
/// locate and return it.
pub fn locate_characteristic(device: &Peripheral) -> Option<Characteristic> {
    device
        .characteristics()
        .iter()
        .find(|v| {
            v.uuid == uuid!("0000ae01-0000-1000-8000-00805f9b34fb")
                && v.properties == CharPropFlags::WRITE
        })
        .cloned()
}

/// The energy to use when printing. Between 0x0000 and 0xFFFF.
const ENERGY: u16 = 0x1000;
/// The quality of the image (used by the printer). Between 0 and 5.
const QUALITY: u8 = 5;
/// The drawing mode to use.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
#[repr(u8)]
enum DrawingMode {
    /// Image mode.
    #[allow(dead_code, reason = "well this still exists")]
    Image = 0,
    /// Text mode.
    #[allow(dead_code, reason = "well this still exists")]
    Text = 1,
}
/// The drawing mode.
const DRAWING_MODE: DrawingMode = DrawingMode::Text;

/// Set meta attributes about the printer (energy, quality, drawing mode).
pub async fn set_meta(device: &Peripheral, ch: &Characteristic) -> btleplug::Result<()> {
    device
        .write(
            ch,
            &get_message(0xAF, ENERGY.to_be_bytes().to_vec()),
            btleplug::api::WriteType::WithResponse,
        )
        .await?;

    device
        .write(
            ch,
            &get_message(0xA4, vec![QUALITY]),
            btleplug::api::WriteType::WithResponse,
        )
        .await?;

    device
        .write(
            ch,
            &get_message(0xBE, vec![DRAWING_MODE as u8]),
            btleplug::api::WriteType::WithResponse,
        )
        .await?;

    Ok(())
}

#[derive(Clone, Debug)]
pub struct Printer {
    adapter: Adapter,
    device: Option<Peripheral>,
    characteristic: Option<Characteristic>,
}

impl Printer {
    /// Create a new [Printer]. Will wait at least a tenth of a second for printers to be
    /// found. If one is found immediately, returns (Self, true). Otherwise,
    /// returns (Self, false).
    pub async fn new() -> btleplug::Result<(Self, bool)> {
        let manager = Manager::new().await?;
        let adapter = manager.adapters().await?[0].clone();
        adapter.start_scan(ScanFilter { services: vec![] }).await?;

        // wait a tenth second for printers
        smol::Timer::after(Duration::from_millis(100)).await;

        let mut out = Self {
            adapter,
            device: None,
            characteristic: None,
        };

        let found = out.search().await?;

        Ok((out, found))
    }

    /// Search for devices and if one is found connect to it.
    pub async fn search(&mut self) -> btleplug::Result<bool> {
        if self.characteristic.is_some() {
            return Ok(true);
        }
        let mut found = false;
        for peri in self.adapter.peripherals().await? {
            if let Some(char) = locate_characteristic(&peri) {
                peri.connect().await?;
                self.device = Some(peri);
                self.characteristic = Some(char);
                found = true;
            }
        }

        if found {
            set_meta(
                self.device.as_ref().unwrap(),
                self.characteristic.as_ref().unwrap(),
            )
            .await?;
        }

        Ok(found)
    }
    /// Whether a device has been found.
    pub fn found(&self) -> bool {
        self.characteristic.is_some()
    }
    /// Print the provided note.
    pub async fn print(&self, note: Note) -> btleplug::Result<()> {
        draw_image(
            grayscale_image_to_bw_image(fonts::rasterize_str(fonts::HEADER_FONT, 10.0, note.data)),
            self.device.as_ref().unwrap(),
            self.characteristic.as_ref().unwrap(),
        )
        .await?;
        Ok(())
    }
}

impl Drop for Printer {
    fn drop(&mut self) {
        if let Some(dev) = self.device.take() {
            let _ = smol::block_on(dev.disconnect());
        }
    }
}
