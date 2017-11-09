"use strict";

const whenDocumentIsReady = new Promise(resolve => {
    document.addEventListener("DOMContentLoaded", resolve);
});

const addTableOfContentToggleEvent = function () {
    const tableOfContents = document.getElementById("TOC");
    const button = document.getElementById("toggle-nav");
    tableOfContents.classList.add("mobile-hidden");

    button.addEventListener("click", function() {
        const navbar = document.getElementsByClassName("navbar")[0];
        navbar.id = tableOfContents.classList.contains("mobile-hidden") ? "fixed-navbar" : "";
        tableOfContents.classList.toggle("mobile-hidden");
    }, false);
}

whenDocumentIsReady.then(addTableOfContentToggleEvent);
