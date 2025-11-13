# Julia Functional Programming code

This code is for functional programming for julia, inspired by python toolz package.

## Installation

The project follows the standard Julia package layout and can be added through
the package manager:

```julia
] add JuliaFP
```

After installation load the module with `using JuliaFP`.

## 함수형 프로그래밍이란?

함수형 프로그래밍(Functional Programming, FP)은 **함수를 일급 시민(first-class citizen)**으로 다루고, 상태(state)나 부수 효과(side effects)를 최소화하는 선언형 프로그래밍 패러다임. 수학적 함수 개념을 기반으로 하며, 로직을 함수의 조합으로 표현하는 것이 특징인 프로그래밍 패턴

## 모나드에 대한 설명

**모나드(Monad)**는 함수형 프로그래밍에서 자주 사용되는 계산의 컨테이너 패턴이다.
직접 값을 다루는 대신, 값을 안전하게 감싸고 (wrap), 필요한 계산을 연결(bind)하여 처리한다.
이는 예외 처리, 비어 있는 값 처리, 비동기 흐름 제어 등 다양한 상황에서 유용하게 사용됨.

이 라이브러리에서는 Ok / Err 타입을 통해 모나드 패턴을 구현하며, 이를 기반으로 안전한 함수 조합이 가능.

## 각 함수에 대한 설명

* `Ok` : 성공을 나타내는 순수 값 래퍼. 예: `Ok(10)` → 결과 객체
* `Err` : 실패를 나타내는 오류 객체. 예: `Err("fail")` Err에 일반적으로 Exception을 객체를 담는다.
* `is_ok` : `Ok`인지 여부 확인. 예: `is_ok(Ok(1)) == true`, `is_ok(Err("fail")) == false`
* `mmap` : `Result` 타입에 안전하게 함수 적용. 예: `mmap(x -> x+1, Ok(3)) == Ok(4)`
* `cmmap` : `mmap`의 curry 버전. 예: `cmmap(x -> x*2)(Ok(2)) == Ok(4)`
* `flatmmap` : 중첩된 `Result`를 평탄화. 예: `flatmmap(x -> Ok(x+1), Ok(1)) == Ok(2)`. 혹은 Result 객체에 담겨 있는 데이터를 꺼내야 할 경우
* `cflatmmap` : `flatmmap`의 curry 버전
* `branch` : 조건 분기 처리. 예: `branch(x->x>0, x->x+1, x->x-1)(Ok(3)) == Ok(4)`. 첫 번째 인자로 분기 할 condition function, 두 번째로 condition이 true 일경우 함수, 세 번째로 codition이 false 일 경우 함수
* `fork` : 두 함수를 동시 적용하고 결합. 예: `fork(+, x->x, x->x)(Ok(1)) == Ok(2)`
* `fold` : Result 타입을 Ok일 경우와 Err일 경우에 대한 서로 다른 함수로 처리 할 수 있다. 예: `fold(f, g)(Ok(3))`는 f 함수가 적용됨. data pipeline 마지막에 logging 할 때 매우 유용
* `cmap` : Base.map의 curry 버전. 예: `cmap(x->x+1)([1,2]) == [2,3]`
* `cfilter` : Base.filter의 curry 버전. 예: `cfilter(iseven)([1,2,3]) == [2]`
* `csplit` : Base.split의 curry 래퍼. 예: `csplit(",")("a,b") == ["a", "b"]`
* `cjoin` : Base.join의 curry 래퍼. 예: `cjoin(",")(["a","b"]) == "a,b"`
* `creplace` : Base.replace의 curry 래퍼. 인자 값으로 변환할 패어들이 들어감. `creplace("a" => "b", "c" => "d")("ac") == "bd"`
* `cstartswith` : startswith의 curry 래퍼. 예: `cstartswith("a")("abc") == true`
* `cendswith` : endswith의 curry 래퍼
* `curry` : 함수의 호출을 지연시켜주는 FP 핵심 function. 예: `curry((x, y) -> x + y)(1)(2) == 3`
* `cons` : 리스트 앞에 값 추가. 예: `cons(1, [2,3]) == [1,2,3]`
* `cone` : 리스트 뒤에 값 추가. 예: `cone(3, [1,2]) == [1,2,3]`
* `diffseq` : 두 시퀀스 차이 비교. 예: `diffseq([1,2], [1,3]) == [(2, 2, 3)]`
* `dropf` : 앞 n개 제거. 예: `dropf(2, [1,2,3,4]) == [3,4]`
* `dropl` : 뒤 n개 제거. 예: `dropl(2, [1,2,3,4]) == [1,2]`
* `frequencies` : 요소 개수 세기. 예: `frequencies([:a, :b, :a]) == Dict(:a=>2, :b=>1)`
* `geti` : 안전한 인덱싱. 예: `geti(2, [1,2,3]) == 2` 또는 딕셔너리 키 접근
    * `function geti(idx::Int, subject::AbstractArray{T}; default::Union{Nothing, T} = nothing)::T where {T}` : array의 지정 위치의 element를 가져옴.
    * `function geti(idx::T, subject::AbstractDict{T, K}; default::Union{Nothing, K}=nothing)::Union{Nothing, K} where {T, K}` : dictionary의 특정 key값을 가져옴.
    * `function geti(idx::AbstractArray{Int}, subject::AbstractArray{T})::AbstractArray{T} where {T}` : array의 지정된 복수의 element를 가져옴.
    * `function geti(idx::AbstractArray{T}, subject::AbstractDict) where {T}` : Dictionary의 deep level key를 가져옴. 예를 들면 `d = Dict(:a => Dict(:b => "c"))` 라고 했을 때 `geti([:a, :b], d) == "c"`와 같이 깊은 레벨의 데이터를 가져 올 수 있음.
* `groupby`: 기준 함수로 그룹화. 예: `groupby(iseven, [1,2,3]) == Dict(false=>[1,3], true=>[2])`
* `flip` : 함수 인자 순서 뒤집기. 예: `flip((x, y) -> x - y)(2, 10) == 8` 기본적으로 curry이 지원된다.
* `interleave` : 여러 시퀀스를 교차 삽입. 예: `interleave([[1,2], [3,4]]) == [1,3,2,4]`
* `interpose` : 사이에 값 삽입. 예: `interpose(0, [1,2,3]) == [1,0,2,0,3]`
* `isuniq` : 유일 여부 확인. 예: `isuniq([1,2,3]) == true`
* `arrjoin` : SQL join처럼 튜플 시퀀스 결합. `how` 키워드(:inner/:left/:right) 지원
* `mapcat` : map + flatten. 예: `mapcat(x->(x,x), [1,2]) == [1,1,2,2]`
* `merged_sort` : mapcat과 sort 함수의 조합.
    * `function merged_sort(seq::AbstractArray{<:AbstractArray}; key::Function=identity, rev=false)` 기본은 오름차순

* `partition` : 리스트를 n개씩 슬라이딩. 예: `partition(2, [1,2,3,4]) == [(1,2), (3,4)]`
* `pluck` : 딕셔너리/리스트에서 특정 값들 추출. 예: `pluck(:a, [Dict(:a=>1)]) == [1]`
    * `function pluck(ind::T, seqs::AbstractArray{<:AbstractDict{T, K}}) where {T, K}` : Dictionary Array를 받아 특정 key만 선택해서 Array로 반환
    * `function pluck(ind::AbstractArray{Int}, seqs::AbstractArray{<:AbstractArray})` : 2차원 array에서 특정 index 리스트만 tuple로 반환.

* `sliding_window` : 슬라이딩 윈도우로 시퀀스 나누기
* `ffirst` : first 함수가 FP 모듈과 맞지 않을 경우가 많아서(typeof(first)) first 함수를 wrapping
* `flast` : last 함수가 FP 모듈과 맞지 않을 경우가 많아서(typeof(last)) last 함수를 wrapping
* `topk` : 가장 큰 k개 추출. 예: `topk(2, [3,1,4]) == [4,3]`
* `countby` : 기준함수로 값 세기. 예: `countby(iseven, [1,2,2,4]) == Dict(true=>2, false=>1)`
* `dd` : `f -> x -> f(x);return x;` 디버깅 및 logging에 유리한 함수
* `juxt` : 여러 함수 동시에 적용. 예: `juxt(x->x+1, x->x*2)(3) == (4,6)`
* `Memoize` : 메모이제이션 함수 래퍼. 동일 입력 재사용할 수 있음.
* `clear!` : Memoize 결과 초기화
* `assoc` : 딕셔너리에 값 추가. 딥키도 지원
* `disassoc` : 딕셔너리에서 키 제거
* `valmap` : 값에 함수 적용. 예: `valmap(x->x+1, Dict(:a=>1)) == Dict(:a=>2)`
* `valfilter` : 값 조건으로 필터. 예: `valfilter(x->x>1, Dict(:a=>1, :b=>2)) == Dict(:b=>2)`
* `compl` : 부정 조건 함수 생성. 예: `filter(compl(iseven), [1,2]) == [1]`
* `itemmap` : (key, value) 모두에 함수 적용
* `itemfilter` : (key, value) 조건으로 필터
* `keyfilter` : 키 조건으로 필터
* `keymap` : 키 변경. 예: `keymap(x->Symbol("k_", x), Dict(1=>10)) == Dict(:k_1 => 10)`
* `unzip` : `zip` 된 것을 분리
* `tmap` : 멀티스레딩 병렬 map. 배열용
* `tvalmap`: 멀티쓰레드 병렬 map. 딕셔너리용
