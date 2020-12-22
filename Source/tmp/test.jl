

mutable struct Kid
    age :: Int64
    name :: String
end

struct Student
    age_ :: Int64
    name_ :: String
    grade_ :: Int64
end

bob.age = 13
bob = Kid(13, "Bobby")
