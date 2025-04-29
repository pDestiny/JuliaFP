using Test

include("FP.jl")

using .FP

# test cases

@testset verbose = true "Monad TEST Set" begin
    
    test_ok_func(x) = div(x,1)
    test_fail_func(x) = div(x, 0)


    @testset "mmap TEST" begin
        @test FP.mmap(test_ok_func, 3) isa FP.Ok
        @test FP.mmap(test_fail_func, 3) isa FP.Err
    end

    
    @testset "is_ok TEST" begin
        @test FP.is_ok(FP.mmap(test_ok_func, 3)) == true
        @test FP.is_ok(FP.mmap(test_fail_func, 3)) == false
        @test FP.is_ok(FP.Err("something wrong!")) == false
    end

    curry_test_func(x,y,z,w) = x + y + z + w
    @testset "curry TEST" begin
        result = curry_test_func
        @testset for i ∈ 1:4
            if i < 4
                result = FP.curry(result, i)
                @test result isa Function
            else
                result = FP.curry(result, i)
                @test !(result isa Function)
            end
        end

    end
end