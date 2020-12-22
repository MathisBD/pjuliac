

struct Kid
    age :: Int64
    name :: String
end

bob = Kid(13, "bobby")
mike = Kid(13, "bobby")
println(bob == mike)