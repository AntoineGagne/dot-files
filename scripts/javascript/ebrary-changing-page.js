(function () {
    const changePage = function (event) {
        if (event.defaultPrevented) {
            return;
        }
        switch (event.key) {
            case 'ArrowLeft':
                document.querySelector('#tool-pager-prev').click();
                break;
            case 'ArrowRight':
                document.querySelector('#tool-pager-next').click();
                break;
            default:
                return;
        }
        event.preventDefault();
    }

    document.addEventListener('keypress', changePage, true);
})();
