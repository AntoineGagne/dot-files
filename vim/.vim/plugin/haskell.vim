if executable("pointfree")
    setlocal formatprg=pointfree
endif

if executable("haskellman")
    setlocal keywordprg=haskellman
endif

setlocal omnifunc=necoghc#omnifunc
