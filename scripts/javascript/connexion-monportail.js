// Will stop MonPortail from requiring action every now and then
window.setInterval(() => window.localStorage.setItem('mpo.securite.derniereAction', Date.now()), 10000)
