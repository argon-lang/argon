use argon_compiler::{Tube, TubeCollection, TubeCollectionBuilder};
use argon_format::tube as tf;
use std::sync::Arc;

pub fn decode_tube(
    tube: impl Iterator<Item = tf::TubeFileEntry>,
    tube_collection_builder: &TubeCollectionBuilder,
) -> Arc<Tube> {
    todo!()
}
