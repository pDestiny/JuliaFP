using Test

include("FP.jl")

using .FP

# test cases

@testset verbose = true "Monad TEST Set" begin

    test_ok_func(x) = div(x,1)
    test_fail_func(x) = div(x, 0)
    # Ok and Err
    @testset "Ok and Err TEST" begin
        ok = FP.Ok(3)
        err = FP.Err("err")
        @test ok.value == 3 && isnothing(ok.error)
        @test isnothing(err.value) && err.error == "err"
    end

    @testset "is_ok TEST" begin
        @test FP.is_ok(FP.mmap(test_ok_func, FP.Ok(3))) == true
        @test FP.is_ok(FP.mmap(test_fail_func, FP.Ok(3))) == false
        @test FP.is_ok(FP.Err("something wrong!")) == false
    end


    @testset "mmap TEST" begin
        @test FP.mmap(test_ok_func, FP.Ok(3)) isa FP.Ok
        @test FP.mmap(test_fail_func, FP.Ok(3)) isa FP.Err
    end

    @testset "flatmmap TEST" begin
        @test FP.flatmmap(test_ok_func, FP.Ok(3)) == 3
        @test FP.flatmmap(test_fail_func, FP.Ok(3)) isa FP.Err
    end

    @testset "fork TEST" begin
        combine_test_func_f(x) = x
        combine_test_func_g(y) = y
        combine_func(x, y) = x + y
        @test FP.fork(combine_func, combine_test_func_f, combine_test_func_g) isa Function
        @test FP.fork(combine_func, combine_test_func_f, combine_test_func_g)(FP.Ok(3)) isa FP.Ok
        @test FP.fork(combine_func, combine_test_func_f, combine_test_func_g)(FP.Ok(3)).value == 6
        
        # test error function
        combine_test_fail_func_f(x) = div(x, 0)
        @test FP.fork(combine_func, combine_test_fail_func_f, combine_test_func_g)(FP.Ok(3)) isa FP.Err
        
        combine_test_fail_func_g(x) = div(x, 0)
        @test FP.fork(combine_func, combine_test_func_f, combine_test_fail_func_g)(FP.Ok(3)) isa FP.Err
        
        combine_fail(x, y) = throw(ErrorException("something wrong!"))
        @test FP.fork(combine_fail, combine_test_func_f, combine_test_func_g)(FP.Ok(3)) isa FP.Err
    end

    @testset "fold TEST" begin
        on_success(r) = begin println(r.value);return true; end
        on_fail(r) = throw(r.error)

        ok = FP.Ok(3)
        err = FP.mmap(x -> div(x, 0), ok)
        fold_rdy = FP.fold(on_success, on_fail)
        @test fold_rdy(ok) == true
        @test_throws DivideError fold_rdy(err)
    end

    
end

@testset verbose=true "Toolz TEST Set" begin

    @testset "curry TEST" begin
        result = (x,y,w,z) -> sum([x,y,w,z])
        @testset for i ∈ 1:4
            if i < 4
                result = FP.curry(result, i)
                @test result isa Function
            else
                result = FP.curry(result, i)
                @test result == sum([1,2,3,4])
            end
        end

        result = (x, y;addition=nothing) -> isnothing(addition) ? x + y : x + y + addition
        @testset for i ∈ 1:2
            if i < 2
                result = FP.curry(result, i)
                @test result isa Function
            else
                result = FP.curry(result, i, addition=3)
                @test result == 6
            end
        end
    end

    
end

