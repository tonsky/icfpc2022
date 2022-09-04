#![allow(dead_code)]
#![allow(irrefutable_let_patterns)]

extern crate core;

use std::env;
use std::fs::File;
use std::io::BufReader;
use std::path::Path;

use fxhash::FxHashMap;
use image::{Rgba, RgbaImage};
use image::io::Reader as ImageReader;

use crate::transport::PictureData;

mod transport;

type BlockId = String;
type Coord = i32;
type Error = String;
type Score = i32;

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
                if !(l < x && x < r) {
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
                if !(b < y && y < t) {
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

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
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

#[derive(Debug, Clone)]
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

impl Operation {
    pub fn serialize(&self) -> String {
        match self {
            Operation::Color { id, color } => {
                format!("color [{}] [{}, {}, {}, {}]", id, color.r, color.g, color.b, color.a)
            }
            Operation::PCut { id, point } => {
                format!("cut [{}] [{}, {}]", id, point.x, point.y)
            }
            Operation::XCut { id, x } => {
                format!("cut [{}] [X] [{}]", id, x)
            }
            Operation::YCut { id, y } => {
                format!("cut [{}] [Y] [{}]", id, y)
            }
            Operation::Swap { id1, id2 } => {
                format!("swap [{}] [{}]", id1, id2)
            }
            Operation::Merge { id1, id2 } => {
                format!("merge [{}] [{}]", id1, id2)
            }
        }
    }
}

type Log = Vec<Operation>;

#[derive(Debug, Clone)]
pub struct Picture {
    counter: u32,
    width: Coord,
    height: Coord,
    blocks: FxHashMap<BlockId, Block>
}

impl Picture {
    fn initial(width: Coord, height: Coord) -> Self {
        let mut blocks = FxHashMap::default();
        blocks.insert("0".to_string(), Block::Simple { shape: Shape::Rect {
            l: 0,
            b: 0,
            r: width,
            t: height
        }, color: Color::WHITE });
        return Picture {
            counter: 0,
            width,
            height,
            blocks: blocks
        }
    }

    fn from_data(data: &PictureData) -> Self {
        let blocks: FxHashMap<BlockId, Block> = data.blocks.iter().map(|block| {
            let color = {
                let (r, g, b, a) = block.color;
                Color { r, g, b, a }
            };
            let (l, b) = block.bottom_left;
            let (r, t) = block.top_right;
            (block.block_id.clone(), Block::Simple {
                shape: Shape::Rect { l, b, r, t },
                color
            })
        }).collect();
        Picture {
            counter: 0,
            width: data.width,
            height: data.height,
            blocks
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
    image: RgbaImage,
    initial: Picture,
}

impl Problem {
    fn load(problem_id: i32) -> Result<Problem, Error> {
        let reader = ImageReader::open(format!("../resources/{}.png", problem_id)).map_err(|err| err.to_string())?;
        let img = reader.decode().map_err(|err| err.to_string())?;

        let path_str = format!("../{}.initial.json", problem_id);
        let initial_pic_path = Path::new(&path_str);
        let initial_pic = if initial_pic_path.exists() {
            let f = File::open(initial_pic_path).map_err(|err| err.to_string())?;
            let reader = BufReader::new(f);
            let picture_data: PictureData = serde_json::from_reader(reader).map_err(|err| err.to_string())?;
            Picture::from_data(&picture_data)
        } else {
            Picture::initial(img.width() as Coord,
                             img.height() as Coord)
        };
        Ok(Problem {
            image: img.to_rgba8(),
            initial: initial_pic
        })
    }

    fn get_color(&self, point: Point) -> Result<Color, Error> {
        let &Rgba([r, g, b, a]) =  self.image.get_pixel(point.x as u32, self.image.height() - point.y as u32 - 1);
        Ok(Color { r, g, b, a })
    }

    fn average_color(&self, left: Coord, bottom: Coord, right: Coord, top: Coord) -> Color {
        let mut r: i32 = 0;
        let mut g: i32 = 0;
        let mut b: i32 = 0;
        let step = 5;
        let count = (right - left) * (top - bottom) / step / step;
        for x in (left..right).step_by(step as usize) {
            for y in (bottom..top).step_by(step as usize) {
                let p = Point { x: x as Coord, y: y as Coord };
                let c = self.get_color(p).unwrap();
                r += c.r as i32;
                g += c.g as i32;
                b += c.b as i32;
            }
        }
        return Color {
            r: (r / count) as u8,
            g: (g / count) as u8,
            b: (b / count) as u8,
            a: 255
        };
    }

    fn most_frequent_color(&self, left: Coord, bottom: Coord, right: Coord, top: Coord) -> Color {
        let step = 5;
        let mut map = FxHashMap::default();

        for x in (left..right).step_by(step as usize) {
            for y in (bottom..top).step_by(step as usize) {
                let p = Point { x: x as Coord, y: y as Coord };
                let c = self.get_color(p).unwrap();
                *map.entry(c).or_default() += 1;
            }
        }

        let mut res_count = 0;
        let mut res_color = Color::WHITE;

        for (c, count) in map.into_iter() {
            if count > res_count {
                res_count = count;
                res_color = c;
            }
        }

        return res_color;
    }

    fn color(&self, left: Coord, bottom: Coord, right: Coord, top: Coord) -> Color {
        // return self.average_color(left, bottom, right, top);
        return self.most_frequent_color(left, bottom, right, top);
    }

    fn similarity(&self, picture: &Picture) -> Result<u64, String> {
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

fn try_logs(logs: Vec<Log>, problem: &Problem) {
    let mut best_score = None;
    for log in &logs {
        let mut picture = problem.initial.clone();
        let mut cost = 0;
        log.iter().for_each(|op| {
            cost += picture.cost(op.clone()).unwrap();
            picture.apply(op.clone()).unwrap();
        });
        let similarity = problem.similarity(&picture).unwrap();
        let score = similarity + cost;

        if best_score.is_none() || score < best_score.unwrap() {
            best_score = Some(score);
            let strs: Vec<String> = log.iter().map(|op| op.serialize()).collect();
            println!("{}|{}", score, strs.join("|"))
        }
    }
}

fn algo_xcut(problem: &Problem) -> Vec<Log> {
    let step = 10;
    return (step..(400 - 3 * step)).step_by(step).flat_map(|x1| {
        ((x1+step)..(400 - 2 * step)).step_by(step).flat_map(move |x2| {
            ((x2+step)..(400 - 1 * step)).step_by(step).flat_map(move |x3| {
                ((x3+step)..(400 - 0 * step)).step_by(step).map(move |x4| {
                    let c1 = problem.color(0, 0, x1 as i32, 400);
                    let c2 = problem.color(x1 as i32, 0, x2 as i32, 400);
                    let c3 = problem.color(x2 as i32, 0, x3 as i32, 400);
                    let c4 = problem.color(x3 as i32, 0, x4 as i32, 400);
                    let c5 = problem.color(x4 as i32, 0, 400, 400);

                    return vec![Operation::Color { id: "0".to_string(), color: c1 },
                                Operation::XCut { id: "0".to_string(), x: x1 as i32 },
                                Operation::Color { id: "0.1".to_string(), color: c2 },
                                Operation::XCut { id: "0.1".to_string(), x: x2 as i32 },
                                Operation::Color { id: "0.1.1".to_string(), color: c3 },
                                Operation::XCut { id: "0.1.1".to_string(), x: x3 as i32 },
                                Operation::Color { id: "0.1.1.1".to_string(), color: c4 },
                                Operation::XCut { id: "0.1.1.1".to_string(), x: x4 as i32 },
                                Operation::Color { id: "0.1.1.1.1".to_string(), color: c5 }];
                })
            })
        })
    }).collect();
}

fn algo_ycut(problem: &Problem) -> Vec<Log> {
    let step = 10;
    return (step..(400 - 3 * step)).step_by(step).flat_map(|y1| {
        ((y1+step)..(400 - 2 * step)).step_by(step).flat_map(move |y2| {
            ((y2+step)..(400 - 1 * step)).step_by(step).flat_map(move |y3| {
                ((y3+step)..(400 - 0 * step)).step_by(step).map(move |y4| {
                    let c1 = problem.color(0, 0, 400, y1 as i32);
                    let c2 = problem.color(0, y1 as i32, 400, y2 as i32);
                    let c3 = problem.color(0, y2 as i32, 400, y3 as i32);
                    let c4 = problem.color(0, y3 as i32, 400, y4 as i32);
                    let c5 = problem.color(0, y4 as i32, 400, 400);

                    return vec![Operation::Color { id: "0".to_string(), color: c1 },
                                Operation::YCut { id: "0".to_string(), y: y1 as i32 },
                                Operation::Color { id: "0.1".to_string(), color: c2 },
                                Operation::YCut { id: "0.1".to_string(), y: y2 as i32 },
                                Operation::Color { id: "0.1.1".to_string(), color: c3 },
                                Operation::YCut { id: "0.1.1".to_string(), y: y3 as i32 },
                                Operation::Color { id: "0.1.1.1".to_string(), color: c4 },
                                Operation::YCut { id: "0.1.1.1".to_string(), y: y4 as i32 },
                                Operation::Color { id: "0.1.1.1.1".to_string(), color: c5 }];
                })
            })
        })
    }).collect();
}

fn algo_rect(problem: &Problem) -> Vec<Log> {
    let step = 16;
    return (step..(400 - step)).step_by(step).flat_map(|l| {
        ((l+step)..400).step_by(step).flat_map(move |r| {
            (step..(400 - step)).step_by(step).flat_map(move |b| {
                ((b+step)..400).step_by(step).map(move |t| {
                    let c00  = problem.color(0, 0, l as i32, b as i32);
                    let c01  = problem.color(l as i32, 0, 400, b as i32);
                    let c020 = problem.color(l as i32, b as i32, r as i32, t as i32);
                    let c021 = problem.color(r as i32, b as i32, 400, t as i32);
                    let c022 = problem.color(r as i32, t as i32, 400, 400);
                    let c023 = problem.color(l as i32, t as i32, r as i32, 400);
                    let c03  = problem.color(0, b as i32, l as i32, 400);

                    return vec![Operation::Color { id: "0".to_string(), color: c00 },
                                Operation::PCut { id: "0".to_string(), point: Point { x: l as i32, y: b as i32 }},
                                Operation::Color { id: "0.2".to_string(), color: c020 },
                                Operation::PCut { id: "0.2".to_string(), point: Point { x: r as i32, y: t as i32 }},
                                Operation::Color { id: "0.1".to_string(), color: c01 },
                                Operation::Color { id: "0.2.1".to_string(), color: c021 },
                                Operation::Color { id: "0.2.2".to_string(), color: c022 },
                                Operation::Color { id: "0.2.3".to_string(), color: c023 },
                                Operation::Color { id: "0.3".to_string(), color: c03 }];
                })
            })
        })
    }).collect();
}

fn algo_x3y2(problem: &Problem) -> Vec<Log> {
    let step = 40;
    return (step..(400 - step)).step_by(step).flat_map(move |x1| {
        ((x1 + step)..400).step_by(step).flat_map(move |x2| {
            (step..400).step_by(step).flat_map(move |y1| {
                (step..400).step_by(step).flat_map(move |y2| {
                    (step..400).step_by(step).map(move |y3| {
                        let c000  = problem.color(0, 0, x1 as i32, y1 as i32);
                        let c001  = problem.color(0, y1 as i32, x1 as i32, 400);
                        let c0100 = problem.color(x1 as i32, 0, x2 as i32, y2 as i32);
                        let c0101 = problem.color(x1 as i32, y2 as i32, x2 as i32, 400);
                        let c0110 = problem.color(x2 as i32, 0, 400, y3 as i32);
                        let c0111 = problem.color(x2 as i32, y3 as i32, 400, 400);
                        
                        return vec![Operation::Color { id: "0".to_string(), color: c000 },
                                    Operation::XCut  { id: "0".to_string(), x: x1 as i32},
                                    Operation::Color { id: "0.1".to_string(), color: c0100 },
                                    Operation::XCut  { id: "0.1".to_string(), x: x2 as i32},
                                    Operation::Color { id: "0.1.1".to_string(), color: c0110 },
                                    Operation::YCut  { id: "0.0".to_string(), y: y1 as i32},
                                    Operation::Color { id: "0.0.1".to_string(), color: c001 },
                                    Operation::YCut  { id: "0.1.0".to_string(), y: y2 as i32},
                                    Operation::Color { id: "0.1.0.1".to_string(), color: c0101 },
                                    Operation::YCut  { id: "0.1.1".to_string(), y: y3 as i32},
                                    Operation::Color { id: "0.1.1.1".to_string(), color: c0111 }];
                    })
                })
            })
        })
    }).collect();
}

fn algo_x3y3(problem: &Problem) -> Vec<Log> {
    let step = 50;
    return (step..(400 - step)).step_by(step).flat_map(move |x1| {
        ((x1 + step)..400).step_by(step).flat_map(move |x2| {
            (step..(400 - step)).step_by(step).flat_map(move |y1| {
                ((y1 + step)..400).step_by(step).flat_map(move |y2| {
                    (step..(400 - step)).step_by(step).flat_map(move |y3| {
                        ((y3 + step)..400).step_by(step).flat_map(move |y4| {
                            (step..(400 - step)).step_by(step).flat_map(move |y5| {
                                ((y5 + step)..400).step_by(step).map(move |y6| {

                                    let c000   = problem.color(0, 0, x1 as i32, y1 as i32);
                                    let c0010  = problem.color(0, y1 as i32, x1 as i32, y2 as i32);
                                    let c0011  = problem.color(0, y2 as i32, x1 as i32, 400);

                                    let c0100  = problem.color(x1 as i32, 0, x2 as i32, y1 as i32);
                                    let c01010 = problem.color(x1 as i32, y1 as i32, x2 as i32, y2 as i32);
                                    let c01011 = problem.color(x1 as i32, y2 as i32, x2 as i32, 400);

                                    let c0110  = problem.color(x2 as i32, 0, 400, y1 as i32);
                                    let c01110 = problem.color(x2 as i32, y1 as i32, 400, y2 as i32);
                                    let c01111 = problem.color(x2 as i32, y2 as i32, 400, 400);
                        
                                    return vec![
                                        Operation::Color { id: "0".to_string(), color: c000 },
                                        Operation::XCut  { id: "0".to_string(), x: x1 as i32},
                                        Operation::Color { id: "0.1".to_string(), color: c0100 },
                                        Operation::XCut  { id: "0.1".to_string(), x: x2 as i32},
                                        Operation::Color { id: "0.1.1".to_string(), color: c0110 },

                                        Operation::YCut  { id: "0.0".to_string(), y: y1 as i32},
                                        Operation::Color { id: "0.0.1".to_string(), color: c0010 },
                                        Operation::YCut  { id: "0.0.1".to_string(), y: y2 as i32},
                                        Operation::Color { id: "0.0.1.1".to_string(), color: c0011 },

                                        Operation::YCut  { id: "0.1.0".to_string(), y: y3 as i32},
                                        Operation::Color { id: "0.1.0.1".to_string(), color: c01010 },
                                        Operation::YCut  { id: "0.1.0.1".to_string(), y: y4 as i32},
                                        Operation::Color { id: "0.1.0.1.1".to_string(), color: c01011 },

                                        Operation::YCut  { id: "0.1.1".to_string(), y: y5 as i32},
                                        Operation::Color { id: "0.1.1.1".to_string(), color: c01110 },
                                        Operation::YCut  { id: "0.1.1.1".to_string(), y: y6 as i32},
                                        Operation::Color { id: "0.1.1.1.1".to_string(), color: c01111 }
                                    ];
                                })
                            })
                        })
                    })
                })
            })
        })
    }).collect();
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let num: i32 = args[1].parse().expect("Wanted a number");

    let problem = Problem::load(num).unwrap();
    if "xcut" == args[2] {
        try_logs(algo_xcut(&problem), &problem);
    } else if "ycut" == args[2] {
        try_logs(algo_ycut(&problem), &problem);
    } else if "rect" == args[2] {
        try_logs(algo_rect(&problem), &problem);
    } else if "x3y2" == args[2] {
        try_logs(algo_x3y2(&problem), &problem);
    } else if "x3y3" == args[2] {
        try_logs(algo_x3y3(&problem), &problem);
    } else {
        panic!("Unknown algorithm {}", args[2]);
    }
}
