
macro_rules! eq {
	($a: expr, $b: expr) => {
		{
			let a = $a;
			let b = $b;
			log::debug!("eq!({}, {}) -- {:?} == {:?}", stringify!($a), stringify!($b), &a, &b);
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
			log::debug!("assert_prop!({}, {})", stringify!($a), stringify!($fn));
			anyhow::ensure!(prop, "Expected property to hold on {:?}: {}", &a, stringify!($fn))
		}
	}
}

pub(crate) use assert_prop;
