let rev = fn(arr) {
	let rev_ = fn(arr,acc) {
		if len(arr) == 0 {
			acc
		} else {
			rev_(rest(arr), cons(acc,first(arr)))
		}
	};

	rev_(arr,[]);
};
