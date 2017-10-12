"use strict";

const whenDocumentIsReady = new Promise(resolve => {
    document.addEventListener("DOMContentLoaded", resolve);
});

const displayCurrentDate = function () {
    const currentDate = new Date(Date.now());
    const currentDateNode = document.getElementById("current-date-text");
    currentDateNode.textContent = currentDate.toDateString();
}

whenDocumentIsReady.then(displayCurrentDate);
