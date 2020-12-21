

function haha(x :: Int64)
    println("int")
end

function haha(y :: Bool)
    println("bool")
end

function haha(x :: String)
    println("string")
end

function f(x)
    haha(x)
end

f("hello")
