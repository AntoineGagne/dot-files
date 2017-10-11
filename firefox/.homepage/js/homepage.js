document.addEventListener("DOMContentLoaded", function () {
    const currentDate = new Date(Date.now()).toLocaleDateString();
    document.getElementById("current-date-text").textContent = currentDate;
});
