//! Font rasterization with fontdue.

/// The bold font.
pub const BOLD_FONT: (&[u8], f64) = (include_bytes!("../fonts/ttf/JetBrainsMonoNL-Bold.ttf"), 1.0);

/// The bold font.
pub const HEADER_FONT: (&[u8], f64) = (
    include_bytes!("../fonts/ttf/JetBrainsMonoNL-BoldItalic.ttf"),
    3.0,
);

/// The default font.
pub const DEFAULT_FONT: (&[u8], f64) = (
    include_bytes!("../fonts/ttf/JetBrainsMonoNL-Medium.ttf"),
    1.0,
);

/// Rasterize a `char` to a bitmap.
pub fn rasterize_char(font: (&'static [u8], f64), size: f64, c: char) -> Vec<Vec<u8>> {
    let font_real = fontdue::Font::from_bytes(font.0, fontdue::FontSettings::default()).unwrap();

    #[expect(
        clippy::cast_possible_truncation,
        reason = "we have bigger problems if it does"
    )]
    let (metrics, bitmap) = font_real.rasterize(c, (size * font.1) as f32);
    bitmap.chunks(metrics.width).map(<[u8]>::to_vec).collect()
}

/// Rasterize a string of characters to a bitmap.
pub fn rasterize_str(font: (&'static [u8], f64), size: f64, s: impl AsRef<str>) -> Vec<Vec<u8>> {
    let mut out = Vec::new();
    for c in s.as_ref().chars() {
        out.append(&mut rasterize_char(font, size, c));
    }
    out
}