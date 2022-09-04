use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Debug)]
pub struct BlockData {
    #[serde(alias = "blockId")]
    pub block_id: String,
    #[serde(alias = "bottomLeft")]
    pub bottom_left: (i32, i32),
    #[serde(alias = "topRight")]
    pub top_right: (i32, i32),
    pub color: (u8, u8, u8, u8)
}

#[derive(Serialize, Deserialize, Debug)]
pub struct PictureData {
    pub width: i32,
    pub height: i32,
    pub blocks: Vec<BlockData>
}

#[test]
fn test() {
    let picture_data: PictureData = serde_json::from_str(r#"
        { "width": 400, "height": 400, "blocks": [{"blockId": "0", "bottomLeft": [0, 0], "topRight": [20, 20], "color": [0, 74, 173, 255]}]}
        "#).unwrap();
    println!("{:?}", picture_data)
}