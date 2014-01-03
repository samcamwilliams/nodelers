%% The state of the page.
-record(pstate, {
	userid = undefined,
	nointro = false,
	token = undefined,
	username = undefined,
	entry = undefined,
	arg = undefined,
	ip = undefined,
	logid = undefined
}).
