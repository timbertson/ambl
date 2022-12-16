
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


macro_rules! assert_prop {
	($a: expr, $fn: expr) => {
		{
			let a = $a;
			let prop = $fn(&a);
			anyhow::ensure!(prop, "Expected property to hold on {:?}:\n{}", &a, stringify!($fn))
		}
	}
}

pub(crate) use assert_prop;
