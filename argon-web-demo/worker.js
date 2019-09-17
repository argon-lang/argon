
console.log = (function() {
	const oldLog = console.log;

	return function(...args) {
		oldLog("Got message");
		oldLog(...args);
	};
})();

onmessage = function(e) {
	console.log("Message received");
	postMessage("Output");
}