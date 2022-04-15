use ompas_rae_structs::exec_context::rae_state::LState;
use sompas_structs::lvalues::LValueS;

#[test]
fn test_union() {
    let mut s1: LState = Default::default();
    s1.insert(LValueS::Int(10), LValueS::Symbol("dix".to_string()));

    let s2: LState = Default::default();
    s1.insert(LValueS::Int(20), LValueS::Symbol("vingt".to_string()));

    let s3 = s1.union(&s2);

    assert_eq!(
        LValueS::Symbol("dix".to_string()),
        *s3.inner.get(&LValueS::Int(10)).unwrap()
    );
    assert_eq!(
        LValueS::Symbol("vingt".to_string()),
        *s3.inner.get(&LValueS::Int(20)).unwrap()
    );
}
