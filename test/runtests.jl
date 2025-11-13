using Test
using JuliaFP

const FP = JuliaFP

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
            ends   = FP.cendswith("world")
            @test starts(str) == true
            @test ends(str)   == true

            # Negative cases
            @test FP.cstartswith("world")(str) == false
            @test FP.cendswith("hello")(str) == false

            # SubString input
            full_sub = SubString(str, 1, length(str))
            @test starts(full_sub) == true
            @test ends(full_sub)   == true

            # Empty prefix/suffix
            @test FP.cstartswith("")(str) == true
            @test FP.cendswith("")(str)   == true

            # Regex prefix/suffix
            regex_start = FP.cstartswith(r"^hello")
            regex_end   = FP.cendswith(r"world$")
            @test regex_start(str) == true
            @test regex_end(str)   == true

            # No match with regex
            @test FP.cstartswith(r"^WORLD")(str) == false
            @test FP.cendswith(r"HELLO$")(str)   == false
        end
    end
    @testset "diffseq TEST" begin
        @test FP.diffseq([1,2,3], [1,2,4]) == [(3, 3, 4)]
        @test FP.diffseq([1,2,3,4], [1,3,2,4]) == [(2, 2, 3), (3, 3, 2)]
        @test FP.diffseq([1,2,3], [1,2,3]) == []
        @test FP.diffseq([], []) == []
    end

    @testset "dropf TEST" begin
        # 기본 동작
        @test FP.dropf(2, [1,2,3,4,5]) == [3,4,5]
        
        # 0개 드롭: 전체 유지
        @test FP.dropf(0, [1,2,3]) == [1,2,3]
        
        # 전체 드롭
        @test FP.dropf(3, [1,2,3]) == []
        
        # n이 길이보다 큰 경우 에러
        @test_throws DomainError FP.dropf(5, [1,2])
        
        # 원본 시퀀스 변경되지 않음 확인
        seq = [10,20,30]
        _ = FP.dropf(1, seq)
        @test seq == [10,20,30]
    end
    
    @testset "dropl TEST" begin
        # 기본 동작
        @test FP.dropl(2, [1,2,3,4,5]) == [1,2,3]
        
        # 0개 드롭: 전체 유지
        @test FP.dropl(0, [1,2,3]) == [1,2,3]
        
        # 전체 드롭
        @test FP.dropl(3, [1,2,3]) == []
        
        # n이 길이보다 큰 경우 에러
        @test_throws DomainError FP.dropl(5, [1,2])
        
        # 원본 시퀀스 변경되지 않음 확인
        seq = [10,20,30]
        _ = FP.dropl(1, seq)
        @test seq == [10,20,30]
    end

    @testset "frequencies TEST" begin
        # 기본 케이스: 숫자 배열
        @test FP.frequencies([1,2,2,3,3,3]) == Dict(1 => 1, 2 => 2, 3 => 3)
    
        # 문자 배열
        @test FP.frequencies(["a", "b", "a", "c", "b", "a"]) == Dict("a" => 3, "b" => 2, "c" => 1)
    
        # 혼합된 데이터 없음: 빈 딕셔너리
        @test FP.frequencies([]) == Dict()
    
        # 하나의 값만 반복
        @test FP.frequencies([:x, :x, :x]) == Dict(:x => 3)
    
        # 유일값만 존재
        @test FP.frequencies(["x", "y", "z"]) == Dict("x" => 1, "y" => 1, "z" => 1)
    end

    @testset "geti TEST" begin
        # 1) Single index on array
        arr = [10, 20, 30]
        @test FP.geti(1, arr) == 10
        @test FP.geti(3, arr) == 30
        @test_throws BoundsError FP.geti(4, arr)
        @test FP.geti(4, arr; default=99) == 99

        # 2) Single key on dict
        dict = Dict("a" => 1, "b" => 2)
        @test FP.geti("a", dict) == 1
        @test FP.geti("c", dict) === nothing
        @test FP.geti("c", dict; default=0) == 0

        # 3) Multiple indices on array
        @test FP.geti([1, 3], arr) == [10, 30]
        @test_throws BoundsError FP.geti([1, 4], arr)

        # 4) Multiple keys on dict
        d = Dict(:x => 10, :y => 20)
        result1 = FP.geti([:x, :z], d)
        @test result1 == Dict(:x => 10, :z => nothing)
        result2 = FP.geti([:x, :z], d; default=-1)
        @test result2 == Dict(:x => 10, :z => -1)
    end

    @testset "groupby TEST" begin
        # 1) Group numbers by parity
        seq = [1, 2, 3, 4, 5]
        result = FP.groupby(x -> iseven(x) ? :even : :odd, seq)
        @test sort(result[:even]) == [2, 4]
        @test sort(result[:odd])  == [1, 3, 5]

        # 2) Empty sequence returns empty dict
        @test FP.groupby(identity, Int[]) == Dict{Any, Array{Int,1}}()

        # 3) Group strings by length
        strs = ["a", "bc", "de", "fgh", "ij", "k"]
        result2 = FP.groupby(length, strs)
        @test sort(result2[1]) == ["a", "k"]
        @test sort(result2[2]) == ["bc", "de", "ij"]
        @test result2[3] == ["fgh"]

        # 4) Group by identity on mixed types
        mixed = [1, "1", 1, "2", "2"]
        result3 = FP.groupby(x -> x, mixed)
        @test result3[1] == [1, 1]
        @test result3["1"] == ["1"]
        @test result3["2"] == ["2", "2"]
    end
    # flip is not that complete function aganist Base Function like plus(+, ÷, -, *). Theses functions must be redefined in FP module
    @testset "flip TEST" begin
        # 1) Basic two-arg function
        f(a, b) = a - b
        flipped = FP.flip(f)
        @test flipped(2, 5) == f(5, 2)

        # 2) Partial application
        flipped2 = FP.flip(f, 10)
        @test flipped2(3) == f(3, 10)

        # 3) Multiple args
        g(a, b, c) = (a, b, c)
        flipped3 = FP.flip(g, 1)
        @test flipped3(2, 3) == g(3, 2, 1)

        # 4) Keyword args support
        h(x, y; z=0) = x + y + z
        flipped_h = FP.flip(h)
        @test flipped_h(2, 5; z=4) == h(5, 2; z=4)
        flipped_h2 = FP.flip(h, 1; z=2)
        @test flipped_h2(3) == h(3, 1; z=2)

        # 5) Error on Base function
        @test_throws ErrorException FP.flip(Base.join, 1)
    end

    @testset "interleave TEST" begin
        @test FP.interleave([[1,2,3], [4,5,6]]) == [1,4,2,5,3,6]
        @test FP.interleave([[1,2], [3,4,5]]) == [1,3,2,4,5]
        @test FP.interleave([[1,2,3]]) == [1,2,3]
        @test FP.interleave([]) == []
        @test FP.interleave([[1,2], []]) == [1,2]
        @test FP.interleave([[], [3,4]]) == [3,4]
    end

    @testset "interpose TEST" begin
        @test FP.interpose(0, [1,2,3]) == [1,0,2,0,3]
        @test FP.interpose("z", ["a","b"]) == ["a","z","b"]
        @test FP.interpose("-", ["solo"]) == ["solo"]
        @test FP.interpose(1, Int[]) == Int[]
    end

    @testset "arrjoin TEST" begin
        
        @test FP.arrjoin([(1,"a"),(2,"b")], [(1,"x"),(3,"y")], x->x[1], y->y[1]) ==
              [((1,"a"),(1,"x"))]
        @test FP.arrjoin([(1,"a"),(2,"b")], [(1,"x"),(3,"y")], x->x[1], y->y[1]; how=:left) ==
              [((1,"a"),(1,"x")), ((2,"b"), (nothing, nothing))]
        @test FP.arrjoin([(1,"a"),(2,"b")], [(1,"x"),(3,"y")], x->x[1], y->y[1]; how=:right) ==
              [((1,"a"),(1,"x")), ((nothing, nothing), (3,"y"))]
    end

    @testset "mapcat TEST" begin
        @test FP.mapcat(x->x*2, [1,2,3]) == [2,4,6]
        @test FP.mapcat(x->x, [[1,2], [3], [[4]]]) == [1,2,3,4]
    end

    @testset "merged_sort TEST" begin
        @test FP.merged_sort([[3,1], [2,5,4]]) == [1,2,3,4,5]
        @test FP.merged_sort([[3,1], [2,5,4]]; rev=true) == [5,4,3,2,1]
        @test FP.merged_sort([[3,1], [2,5,4]]; key=x->-x) == [5,4,3,2,1]
    end

    @testset "partition TEST" begin
        @test FP.partition(3, [1,2,3,4,5,6,7]) == [(1,2,3), (4,5,6)]
        @test FP.partition(3, Int[]) == Tuple[]
        @test FP.partition(3, [1,2,3,4]; pad=0) == [(1,2,3), (4,0,0)]
    end

    @testset "pluck TEST" begin
        @test FP.pluck(:a, [Dict(:a=>1), Dict(:a=>2, :b=>3)]) == [1,2]
        @test FP.pluck([1,3], [[10,20,30], [40,50,100]]) == [(10,30), (40, 100)]
        @test_throws BoundsError FP.pluck([1,3], [[10,20,30], [40,50]]) == [(10,30), (40, nothing)]
    end

    @testset "sliding widnow TEST" begin
        @test FP.sliding_window(3, [1,2,3,4,5]) == [[1,2,3], [2,3,4], [3,4,5]]
        @test FP.sliding_window(2, "abcd") == ["ab","bc","cd"]
    end

    @testset "countby TEST" begin
        @test FP.countby(x->iseven(x), [1,2,2,3,4]) == Dict(false=>2, true=>3)
    end

    @testset "dd TEST" begin
        @test FP.dd(x->nothing)(5) == 5
    end

    @testset "juxt TEST" begin
        @test FP.juxt(x->x+1, x->x*2)(3) == (4,6)
        @test FP.juxt([x->x+1, x->x*2])(3) == (4,6)
    end

    @testset "Memoize TEST" begin
        counter = Ref(0)
        f = x -> (counter[] += 1; x*x)
        m = FP.Memoize(f)
        @test m(2) == 4
        @test m(2) == 4
        @test counter[] == 1
        @test length(keys(m.memory)) == 1
        FP.clear!(m)
        @test isempty(keys(m.memory))
    end

    @testset "assoc TEST" begin
        @test FP.assoc(Dict(:a=>1), :b, 2) == Dict(:a=>1, :b=>2)
        @test FP.assoc(Dict(:a=>Dict(:b=>1)), [:a, :c], 2) == Dict(:a=>Dict(:b=>1, :c=>2))
    end

    @testset "disassoc TEST" begin
        @test FP.disassoc(Dict(:a=>1, :b=>2), :a) == Dict(:b=>2)
        @test FP.disassoc(Dict(:a=>1, :b=>2), :a, :b) == Dict()
    end

    @testset "valmap TEST" begin
        @test FP.valmap(x->x*2, Dict(:a=>1, :b=>2)) == Dict(:a=>2, :b=>4)
    end

    @testset "valfilter TEST" begin
        @test FP.valfilter(x->x>1, Dict(:a=>1, :b=>2, :c=>3)) == Dict(:b=>2, :c=>3)
    end

    @testset "compl TEST" begin
        @test FP.compl(x->x>0)(-1) == true
        @test FP.compl(x->x>0)(1) == false
    end

    @testset "itemmap TEST" begin
        @test FP.itemmap(p->p[1]=>p[2]*2, Dict(:a=>2, :b=>3)) == Dict(:a=>4, :b=>6)
    end

    @testset "itemfilter TEST" begin
        @test FP.itemfilter(p->p[2]>2, Dict(:a=>1, :b=>3)) == Dict(:b=>3)
    end

    @testset "keyfilter TEST" begin
        @test FP.keyfilter(k->k>1, Dict(1=>10, 2=>20, 3=>30)) == Dict(2=>20, 3=>30)
    end

    @testset "keymap TEST" begin
        @test FP.keymap(k->k*2, Dict(1=>10, 2=>20)) == Dict(2=>10, 4=>20)
    end

    @testset "unzip TEST" begin
        @test FP.unzip(zip([1,2], [3,4])) == [(1,3), (2,4)]
    end

    @testset "tmap TEST" begin
        function slow_square(x)
            sleep(0.1)  # 일부러 딜레이를 줌
            return x^2
        end
        data = collect(1:10)
        @test tmap(slow_square, data) == [1, 4, 9, 16, 25, 36, 49, 64, 81, 100]
    end
end
