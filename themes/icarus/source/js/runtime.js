function siteRuntime(startDate) {
	const start = new Date(startDate);
	const now = new Date();
	const elapsed = now - start;
	
	const days = Math.floor(elapsed / (1000 * 60 * 60 * 24));
    const hours = Math.floor((elapsed % (1000 * 60 * 60 * 24)) / (1000 * 60 * 60));
    const minutes = Math.floor((elapsed % (1000 * 60 * 60)) / (1000 * 60));
	const seconds = Math.floor((elapsed % (1000 * 60)) / 1000);
	
	return `本站已安全运行${days}天${hours}小时${minutes}分钟${seconds}秒`;
}

function updateRuntime() {
	const startDate = '2019-06-01T00:00:00';
	const runtimeElement = document.getElementById('site-runtime');
	if (runtimeElement) {
		runtimeElement.innerText = siteRuntime(startDate);
	}
}

setInterval(updateRuntime, 1000);