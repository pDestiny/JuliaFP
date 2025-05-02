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

    @testset "cons & cone TEST" begin
        # cons test
        @test FP.cons("a", "abc") == "aabc"
        @test FP.cons(Base.split("abc", "")[1], "abc") == "aabc"
        @test FP.cons("a", ["a", "b", "c"]) == ["a", "a", "b", "c"]
        @test FP.cons(1, [1,2,3]) == [1,1,2,3]

        # cone test
        @test FP.cone("a", "abc") == "abca"
        @test FP.cone(Base.split("abc", "")[1], "abc") == "abca"
        @test FP.cone("a", ["a", "b", "c"]) == [ "a", "b", "c", "a"]
        @test FP.cone(1, [1,2,3]) == [1,2,3,1]
    end

    @testset verbose = true "Base Function Wrapper Test" begin


        # cmap
        @testset "cmap TEST" begin
            @test FP.cmap(Base.identity)([1,2,3]) == [1,2,3]
            @test [1,2, 3] |> FP.cmap(Base.identity) == [1,2,3]
            @test 1:10 |> FP.cmap(identity) == [1:10...]     
        end
        

        # cfilter
        @testset "cfilter TEST" begin
            @test FP.cfilter((x) -> true)([1,2,3]) == [1,2,3]
            @test FP.cfilter(iseven)([1,2,3]) == [2]
            @test 1:10 |> FP.cfilter(iseven) == [2,4,6,8,10]    
        end
        
        # cjoin
        @testset "cjoin TEST" begin
            # 1) 기본 String 배열
            seq_str = ["foo", "bar", "baz"]
            @test FP.cjoin("-")(seq_str) == "foo-bar-baz"

            # 2) SubString 배열 (split 함수 결과)
            raw = "one,two,three"
            seq_sub = split(raw, ",")             # Vector{SubString}
            @test eltype(seq_sub) <: SubString     # 확인용
            @test FP.cjoin(";")(seq_sub) == "one;two;three"

            # 3) 빈 배열 처리
            empty_str = String[]
            @test FP.cjoin("|")(empty_str) == ""

            empty_sub = SubString{String}[]       # 혹은 split("", ",")
            @test FP.cjoin("|")(empty_sub) == ""

            # 4) 단일 요소
            single_str = ["solo"]
            @test FP.cjoin("+")(single_str) == "solo"

            single_sub = split("solo", "s")       # ["", "olo"] SubString
            @test FP.cjoin("s")(single_sub) == "solo"

            # 5) 숫자 문자열도 동작 확인
            num_sub = split("1-2-3-4", "-")
            @test FP.cjoin("_")(num_sub) == "1_2_3_4"

            @test FP.cjoin('-')(split(raw, ",")) == "one-two-three"
        end
        # csplit
        # 1) Basic String input
        @testset "csplit TEST" begin
            raw_str = "foo,bar,baz"
            @test FP.csplit(",")(raw_str) == ["foo", "bar", "baz"]
    
            # 2) SubString input (full string)
            raw_sub = "one;two;three"
            full_sub = SubString(raw_sub, 1, length(raw_sub))
            result_sub = FP.csplit(";")(full_sub)
            @test result_sub == ["one", "two", "three"]
            @test eltype(result_sub) <: SubString
    
            # 3) Char delimiter
            raw_char = "a-b-c"
            @test FP.csplit('-')(raw_char) == ["a", "b", "c"]
    
            # 4) Empty string
            @test FP.csplit(",")("") == [""]
    
            # 5) No delimiter present
            @test FP.csplit("|")("abc") == ["abc"]
    
            @test FP.csplit(';')(raw_sub) == result_sub 
        end
        

        # creplace
        @testset "creplace TEST" begin
            # 1) Simple string replacement
            raw = "foo bar foo"
            replacer = FP.creplace("foo" => "baz")
            @test replacer(raw) == "baz bar baz"

            # 2) Multiple pairs and SubString input
            raw2 = "a-b-c-d"
            subs2 = SubString(raw2, 1, length(raw2))
            replacer2 = FP.creplace('-' => '_', "c" => "C")
            @test replacer2(raw2) == "a_b_C_d"
            @test replacer2(subs2) == "a_b_C_d"

            # 3) No matches leaves string unchanged
            nochange = "hello world"
            @test FP.creplace("x" => "y")(nochange) == nochange

            # 4) Empty string
            @test FP.creplace("anything" => "whatever")("") == ""
        end

        @testset "cstartswith & cendswith TEST" begin
            # Basic String tests
            str = "hello_world"
            starts = FP.cstartswith("hello")
            ends   = FP.cendsswith("world")
            @test starts(str) == true
            @test ends(str)   == true

            # Negative cases
            @test FP.cstartswith("world")(str) == false
            @test FP.cendsswith("hello")(str) == false

            # SubString input
            full_sub = SubString(str, 1, length(str))
            @test starts(full_sub) == true
            @test ends(full_sub)   == true

            # Empty prefix/suffix
            @test FP.cstartswith("")(str) == true
            @test FP.cendsswith("")(str)   == true

            # Regex prefix/suffix
            regex_start = FP.cstartswith(r"^hello")
            regex_end   = FP.cendsswith(r"world$")
            @test regex_start(str) == true
            @test regex_end(str)   == true

            # No match with regex
            @test FP.cstartswith(r"^WORLD")(str) == false
            @test FP.cendsswith(r"HELLO$")(str)   == false
        end

        
    end
end

