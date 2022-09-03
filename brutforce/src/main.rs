#![allow(dead_code)]
#![allow(irrefutable_let_patterns)]
extern crate core;

use std::collections::HashMap;
use image::io::Reader as ImageReader;
use image::{Rgba, RgbaImage};

type BlockId = String;
type Coord = i32;
type Error = String;

#[derive(Debug, Copy, Clone)]
enum Shape {
    Rect {
        l: Coord,
        b: Coord,
        r: Coord,
        t: Coord
    }
}

impl Shape {
    pub fn square(size: Coord) -> Shape {
        Shape::Rect {
            l: 0,
            b: 0,
            r: size,
            t: size
        }
    }

    pub fn pcut(&self, p: Point) -> Result<Vec<Shape>, Error> {
        let Point { x, y } = p;
        match *self {
            Shape::Rect { l, b, r, t } => {
                if !(x < r && l < x && y < t && b < y) {
                    return Err(format!("Failed to pcut {:?} doesn't contain {:?}", self, &p));
                }
                Ok(vec![Shape::Rect { l: l, b: b, r: x, t: y },
                        Shape::Rect { l: x, b: b, r: r, t: y },
                        Shape::Rect { l: x, b: y, r: r, t: t },
                        Shape::Rect { l: l, b: y, r: x, t: t }])
            }
        }
    }

    pub fn xcut(&self, x: Coord) -> Result<Vec<Shape>, Error> {
        match *self {
            Shape::Rect { l, b, r, t } => {
                if !(x < r && l < x) {
                    return Err(format!("Failed to xcut {:?} doesn't contain {:?}", self, x));
                }
                Ok(vec![Shape::Rect { l: l, b: b, r: x, t: t },
                        Shape::Rect { l: x, b: b, r: r, t: t }])
            }
        }
    }

    pub fn ycut(&self, y: Coord) -> Result<Vec<Shape>, Error> {
        match *self {
            Shape::Rect { l, b, r, t } => {
                if !(t < t && b < y) {
                    return Err(format!("Failed to ycut {:?} doesn't contain {:?}", self, y));
                }
                Ok(vec![Shape::Rect { l: l, b: b, r: r, t: y },
                        Shape::Rect { l: l, b: y, r: r, t: t }])
            }
        }
    }

    pub fn intersect(&self, other: &Shape) -> Option<Shape> {
        match *self {
            Shape::Rect { l: l1, b: b1, r: r1, t: t1 } => {
                match *other {
                    Shape::Rect { l: l2, b: b2, r: r2, t: t2 } => {
                        if (r1 <= l2) || (r2 <= l1) || (t1 <=b2) || (t2 <= b1) {
                            None
                        } else {
                            Some(Shape::Rect {
                                l: l1.max(l2),
                                b: b1.max(b2),
                                r: r1.min(r2),
                                t: t1.min(t2)
                            })
                        }
                    }
                }
            }
        }
    }

    pub fn widht(&self) -> Coord {
        match self {
            Shape::Rect { r, l, .. } => {
                r - l
            }
        }
    }

    pub fn height(&self) -> Coord {
        match self {
            Shape::Rect { t, b, .. } => {
                t - b
            }
        }
    }

    pub fn is_same(&self, other: &Self) -> bool {
        self.height() == other.height() && self.widht() == other.widht()
    }

    pub fn merge(&self, other: &Self) -> Option<Shape> {
        if let Shape::Rect { l: l1, b: b1, r: r1, t: t1 } = *self {
            if let Shape::Rect { l: l2, b: b2, r: r2, t: t2 } = *other {
                // shape1
                // shape2
                if (b1 == t2) && (l1 == l2) && (r1 == r2) {
                    return Some(Shape::Rect {
                        l: l1,
                        b: b2,
                        r: r1,
                        t: t1
                    })
                }

                // shape2
                // shape1
                if (b2 == t1) && (l1 == l2) && (r1 == r2) {
                    return Some(Shape::Rect {
                        l: l1,
                        b: b1,
                        r: r1,
                        t: t2
                    })
                }

                // shape1 shape2
                if (r1 == l2) && (b1 == b2) && (t1 == t2) {
                    return Some(Shape::Rect {
                        l: l1,
                        b: b1,
                        r: r2,
                        t: t1
                    })
                }

                // shape2 shape1
                if (r2 == l1) && (b1 == b2) && (t1 == t2) {
                    return Some(Shape::Rect {
                        l: l2,
                        b: b1,
                        r: r1,
                        t: t1
                    })
                }
            }
        }
        None
    }

    fn contains(&self, p: Point) -> bool {
        match *self {
            Shape::Rect { l, b, r, t } => {
                l <= p.x && p.x < r && b <= p.y && p.y < t
            }
        }
    }

    fn area(&self) -> Coord {
        self.widht() * self.height()
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
struct Point {
    x: Coord,
    y: Coord
}

#[derive(Debug, Copy, Clone)]
struct Color {
    r: u8,
    g: u8,
    b: u8,
    a: u8
}

impl Color {
    const WHITE: Color = Color {
        r: 255,
        g: 255,
        b: 255,
        a: 255
    };

    const BLACK: Color = Color {
        r: 0,
        g: 0,
        b: 0,
        a: 255
    };

    fn distance(&self, other: &Color) -> f64 {
        let Color { r: r1, g: g1, b: b1, a: _a1 } = *self;
        let Color { r: r2, g: g2, b: b2, a: _a2 } = *other;
        let dr = r1 as f64 - r2 as f64;
        let dg = g1 as f64 - g2 as f64;
        let db = b1 as f64 - b2 as f64;
        // alpha ignored
        return (dr * dr + dg * dg + db * db).sqrt();
    }
}

#[derive(Debug, Clone)]
enum Block {
    Simple {
        shape: Shape,
        color: Color,
    },

    Complex {
        shape: Shape,
        children: Vec<Block>
    }
}

impl Block {
    fn shape(&self) -> Shape {
        *match self {
            Block::Simple { shape, .. } => shape,
            Block::Complex { shape, .. } => shape
        }
    }

    fn update_shape(&self, new_shape: Shape) -> Block {
        match self {
            Block::Simple {  color, .. } => {
                Block::Simple { shape: new_shape, color: *color }
            }
            Block::Complex { children, .. } => {
                Block::Complex { shape: new_shape, children: (*children).clone() }
            }
        }
    }
}

#[derive(Debug)]
enum Operation {
    Color {
        id: BlockId,
        color: Color
    },
    PCut {
        id: BlockId,
        point: Point
    },
    XCut {
        id: BlockId,
        x: Coord
    },
    YCut {
        id: BlockId,
        y: Coord
    },
    Swap {
        id1: BlockId,
        id2: BlockId
    },
    Merge {
        id1: BlockId,
        id2: BlockId
    }
}

#[derive(Debug)]
pub struct Picture {
    counter: u32,
    width: Coord,
    height: Coord,
    blocks: HashMap<BlockId, Block>
}


impl Picture {
    fn initial() -> Self {
        return Picture {
            counter: 0,
            width: 400,
            height: 400,
            blocks: HashMap::from([("0".to_string(),
                                    Block::Simple { shape: Shape::square(400),
                                                    color: Color::WHITE
                                    })
            ])
        }
    }

    fn apply_color(&mut self, id: BlockId, color: Color) -> Result<(), Error> {
        let old_block = self.blocks.remove(&id).ok_or_else(||
            format!("Failed to color: no block with id {}", id))?;
        let new_block = match old_block {
            Block::Simple { shape, .. } => {
                Block::Simple { shape, color }
            }
            Block::Complex { shape, .. } => {
                Block::Simple { shape, color }
            }
        };
        self.blocks.insert(id, new_block);
        Ok(())
    }

    fn apply_cut<F>(&mut self, id: BlockId, shape_cut_fn: F) -> Result<(), Error>
    where F: FnOnce(Shape) -> Result<Vec<Shape>, Error> {
        let old_block = self.blocks.remove(&id).ok_or_else(||
            format!("Failed to PCut: no block with id {}", id))?;
        match old_block {
            Block::Simple { shape, color } => {
                let new_shapes = shape_cut_fn(shape)?;
                let new_blocks = new_shapes.iter().map(|shape| {
                    Block::Simple { shape: (*shape).clone(), color }
                });
                let block_with_ids = new_blocks.enumerate().map(|(child_id, block)| {
                    (format!("{}.{}", id, child_id).to_string(), block)
                });
                self.blocks.extend(block_with_ids);
                Ok(())
            }
            Block::Complex { shape, children } => {
                let new_shapes = shape_cut_fn(shape)?;
                let new_blocks = new_shapes.iter().map(|shape| {
                    let filtered_children = children.iter().filter_map(|child| {
                        match child {
                            Block::Simple { shape: child_shape, color } => {
                                shape.intersect(child_shape).map(|new_shape| {
                                    Block::Simple { shape: new_shape, color: *color }
                                })
                            }
                            _ => todo!("Nested complex blocks")
                        }
                    });
                    Block::Complex { shape: *shape, children: filtered_children.collect() }
                });
                let block_with_ids = new_blocks.enumerate().map(|(child_id, block)| {
                    (format!("{}.{}", id, child_id).to_string(), block)
                });
                self.blocks.extend(block_with_ids);
                Ok(())
            }
        }
    }

    fn apply_swap(&mut self, id1: BlockId, id2: BlockId) -> Result<(), Error> {
        let block1 = self.blocks.get(&id1).ok_or_else(||
            format!("Failed to Swap: no block with id {}", id1))?.clone();
        let block2 = self.blocks.get(&id1).ok_or_else(||
            format!("Failed to Swap: no block with id {}", id2))?.clone();
        let shape1 = block1.shape();
        let shape2 = block2.shape();
        if !shape1.is_same(&shape2) {
            return Err(format!("Failed to Swap: block has different shapes: {:?} and {:?}", &shape1, &shape2))
        }
        self.blocks.insert(id1, block1.update_shape(shape2));
        self.blocks.insert(id2, block2.update_shape(shape1));
        Ok(())
    }

    // fn apply_merge(&mut self, id1: BlockId, id2: BlockId) -> Result<(), Error> {
    //     let block1 = self.blocks.get(&id1).ok_or_else(||
    //         format!("Failed to Merge: no block with id {}", id1))?;
    //     let block2 = self.blocks.get(&id1).ok_or_else(||
    //         format!("Failed to Merge: no block with id {}", id2))?;
    //     let shape1 = block1.shape();
    //     let shape2 = block2.shape();
    //     todo!()
    // }

    fn apply(&mut self, op: Operation) -> Result<(), Error> {
        match op {
            Operation::Color { id, color } => {
                self.apply_color(id, color)
            }
            Operation::PCut { id, point } => {
                self.apply_cut(id, |shape| {
                    shape.pcut(point)
                })
            }
            Operation::XCut { id, x } => {
                self.apply_cut(id, |shape| {
                    shape.xcut(x)
                })
            }
            Operation::YCut { id, y } => {
                self.apply_cut(id, |shape| {
                    shape.ycut(y)
                })
            }
            Operation::Swap { id1, id2 } => {
                self.apply_swap(id1, id2)
            }
            // Operation::Merge { .. } => {}
            _ => todo!()
        }
    }

    fn cost(&self, op: Operation) -> Result<u64, Error> {
        const X_CUT_COST: i32 = 7;
        const Y_CUT_COST: i32 = 7;
        const P_CUT_COST: i32 = 10;
        const COLOR_COST: i32 = 5;
        const SWAP_COST: i32 = 3;
        const MERGE_COST: i32 = 1;

        match op {
            Operation::Color { id, .. } => {
                let block = self.blocks.get(&id).ok_or_else(|| format!("Can't cost Color no block with id: {:?}", id))?;
                let area = block.shape().area();
                let base = COLOR_COST;
                Ok(((base * self.width * self.height) as f64 / area as f64).round() as u64)
            }
            Operation::PCut { id, .. } => {
                let block = self.blocks.get(&id).ok_or_else(|| format!("Can't cost PCut no block with id: {:?}", id))?;
                let area = block.shape().area();
                let base = P_CUT_COST;
                Ok(((base * self.width * self.height) as f64 / area as f64).round() as u64)
            }
            Operation::XCut { id, .. } => {
                let block = self.blocks.get(&id).ok_or_else(|| format!("Can't cost XCut no block with id: {:?}", id))?;
                let area = block.shape().area();
                let base = X_CUT_COST;
                Ok(((base * self.width * self.height) as f64 / area as f64).round() as u64)
            }
            Operation::YCut { id, .. } => {
                let block = self.blocks.get(&id).ok_or_else(|| format!("Can't cost YCut no block with id: {:?}", id))?;
                let area = block.shape().area();
                let base = Y_CUT_COST;
                Ok(((base * self.width * self.height) as f64 / area as f64).round() as u64)
            }
            Operation::Swap { id1, .. } => {
                let block = self.blocks.get(&id1).ok_or_else(|| format!("Can't cost Swap no block with id: {:?}", id1))?;
                let area = block.shape().area();
                let base = SWAP_COST;
                Ok(((base * self.width * self.height) as f64 / area as f64).round() as u64)
            }
            Operation::Merge { id1, id2 } => {
                let block1 = self.blocks.get(&id1).ok_or_else(|| format!("Can't cost Merge no block with id: {:?}", id1))?;
                let block2 = self.blocks.get(&id2).ok_or_else(|| format!("Can't cost Merge no block with id: {:?}", id1))?;
                let area = block1.shape().area() + block2.shape().area();
                let base = MERGE_COST;
                Ok(((base * self.width * self.height) as f64 / area as f64).round() as u64)
            }
        }
    }

    fn get_color(&self, p: Point) -> Result<Color, Error> {
        let block = self.blocks.values().find(|block| {
            block.shape().contains(p)
        }).ok_or_else(|| format!("Malformed Picture: no one of blocks contains {:?}", &p))?;
        match block {
            Block::Simple { color, .. } => {
                Ok(*color)
            }
            Block::Complex { children, .. } => {
                let result = children.iter().find_map(|child_block| {
                    match child_block {
                        Block::Simple { color, shape } => {
                            if shape.contains(p) {
                                Some(Ok(*color))
                            } else {
                                None
                            }
                        },
                        Block::Complex { .. } => Some(Err("Malformed picture: nested complex blocks".to_string()))
                    }
                });
                result.unwrap()
            }
        }
    }
}

struct Problem {
    image: RgbaImage
}

impl Problem {
    fn load(problem_id: i32) -> Result<Problem, Error> {
        let reader = ImageReader::open(format!("../resources/{}.png", problem_id)).map_err(|err| err.to_string())?;
        let img = reader.decode().map_err(|err| err.to_string())?;
        assert!(img.height() == 400 && img.width() == 400);
        Ok(Problem {
            image: img.to_rgba8()
        })
    }

    fn get_color(&self, point: Point) -> Result<Color, Error> {
        let &Rgba([r, g, b, a]) =  self.image.get_pixel(point.x as u32, self.image.height() - point.y as u32 - 1);
        Ok(Color { r, g, b, a })
    }

    fn similarity(self, picture: &Picture) -> Result<u64, String> {
        let mut result = 0f64;
        for x in 0..self.image.width() {
            for y in 0..self.image.height() {
                let p = Point { x: x as Coord, y: y as Coord };
                result += self.get_color(p)?.distance(&picture.get_color(p)?);
            }
        }
        Ok((result * 0.005).round() as u64)
    }
}

fn main() {
    Problem::load(7).unwrap();
    println!("Hello, world!");
}
