
macro_rules! eq {
	($a: expr, $b: expr) => {
		{
			let a = $a;
			let b = $b;
			anyhow::ensure!(a == b, "Expected {:?} == {:?}", &a, &b)
		}
	}
}

pub(crate) use eq;
