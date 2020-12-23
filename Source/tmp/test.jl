

struct Kid
    age :: Int64
    name :: Any
    k :: Nothing
end

struct Student
    age_ :: Int64
    name_ :: String
    grade_ :: Int64
    friend :: Kid
end

mike = Kid(13, "Mike", "hi")
mike2 = Kid(13, "Mike", nothing)

bob = Student(13, "Bobby", 40, mike)
bob2 = Student(13, "Bobby", 40, mike2)
println(bob == bob2)
