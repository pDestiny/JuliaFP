module FP

export Result, is_ok, map, flatmap, curry, branch, fork, fold, diffseq, flip, groupby, interleave

### default Funcional Programming Starts ###

abstract type AbstractResult{T, E} end

struct Ok{T} <: AbstractResult{T, Nothing}
    value::T
end

struct Error{E} <: AbstractResult{Nothing, E} 
    error::E
end

is_ok(r::Ok) = true
is_ok(r::Error) = false

map(f::Function, r::Ok, args...) = begin
    try
        return Ok(f(r.value, args...))
    catch e
        return Error(e)
    end
end

curry(f::Function, args...;kwargs...) = begin
   method = first(methods(f))
   required_args = method.nargs - length(Base.kwarg_decl(method))
   return length(args) >= required_args ? f(args...;kwargs...) : (more_args...;more_kwargs...) -> curry(f, args..., more_args...;kwargs..., more_kwargs...)
end

map(f::Function, r::Error) = r

# 
flatmap(f::Function, r::Ok, args...) = begin
    try
        return f(r.value, args...)
    catch e
        return Error(e)
    end
end

fork(f::Function, g::Function, combine::Function) = (r::AbstractResult, args...) -> combine(f(r, args...), g(r, args...))

fold(r::AbstractResult, on_success::Function, on_fail::Function) = is_ok(r) ? on_success(r) : on_fail(r)

branch(cond::Function, f::Function, g::Function) = (r::AbstractResult, args...) -> cond(r) ? map(f, r, args...) : map(g, r, args...)

### default functional Programming Function Ends ###


### Toolz itertools Implementation ###
function cons(add_target::T, seq::Vector{T}) where {T}
    return [[add_target]; deepcop(seq)]
end

function cone(add_target::T, seq::Vector{T}) where {T}
    return [deepcop(seq); [add_target]]
end

cmap(f::Function) = (itr::AbstractArray) -> Base.map(f, itr)
cfilter(cond::Function) = (itr::AbstractArray) -> Base.filter(cond, itr)

# difference at position compared to seq_A -> 
# return value is (index, different item)
function diffseq(seq_a::Vector{T}, seq_b::Vector{T})::Vector{Tuple{Int, Union{T, Nothing}, Union{T, Nothing}}} where {T}
    length_a, length_b = length(seq_a), length(seq_b)
    maxlen = max(length_a, length_b)
    padded_a = vcat(seq_a, fill(nothing, maxlen - length_a))
    padded_b = vcat(seq_b, fill(nothing, maxlen - length_b))
    indexed_rows = collect(enumerate(zip(padded_a, padded_b)))
    filtered_rows = Base.filter(((i, (a, b)),) -> (a !== nothing && b !== nothing && a != b), indexed_rows)
    return Base.map(((i, (a, b)),) -> (i, a, b), filtered_rows)
end

function dropf(n::Int, seq::AbstractArray{T})::AbstractArray{T} where {T}
    seq = deepcop(seq)
    return n <= length(seq) ? seq[n+1:end] : throw(DomainError("$n is bigger than Sequence size($(length(seq)))"))
end

function dropl(n::Int, seq::AbstractArray{T})::AbstractArray{T} where {T}
    seq = deepcop(seq)
    return n <= length(seq) ? seq[n:length(seq) - 1] : throw(DomainError("$n is bigger than Sequence size($(length(seq)))"))
end

function frequencies(seq::Vector{T})::Dict{T, Int64} where {T}
    seq = deepcop(seq)
    return isempty(seq) ? Dict{T, Int64}() : Dict(Base.map(item -> (item, count((==)(item), seq)), unique(seq)))
end

function geti(idx::Union{Int64, T}, subject::Union{Dict{T, E}, AbstractArray{E}}; default::Union{Nothing, T}=nothing)::Union{Nothing, E} where {T, E}
    if subject isa AbstractArray
        return (1 <= idx <= length(subject)) ? subject[idx] : default
    elseif subject isa Dict
        return get(subject, idx, default)
    end
end

function geti(idx::Vector{Union{Int64, T}}, subject::Union{Dict{T, E}, AbstractArray{E}}; default::Union{Nothing, T}=nothing)::Vector{Union{E, Nothing}} where {T, E}
    if subject isa AbstractArray
        return (1 <= max(idx...) <= length(subject)) ? subject[idx] : throw(DomainError("[$(join(idx, " "))] indexex have out of range item from sequence(length=$(length(idx)))"))
    elseif subject isa Dict
        return Base.map(i -> get(subject, i, default), idx)
    end
end

function groupby(cond::Function, seq::Vector{T})::Dict{Any, Vector{T}} where {T}
    # group data by condition
    return reduce((acc, item) -> merge(vcat, acc, Dict{Any, Vector{T}}(cond(item) => [item])), seq, init=Dict{Any, Vector{T}}())
end

# unfortunately, julia's dispatch system doesn't allow easy pizy fliping.
function flip(f::Function, args...; kwargs...)::Union{Function, Any}
    m = first(methods(f))
    if occursin("Base", string(m.module))
        throw(ErrorException("Base Module function is not available for flip function. Sorry!"))
    end
    num_args = m.nargs - length(Base.kwarg_decl(m)) - 1
    if num_args <= length(args)
        return f(reverse(args[1:num_args])...;kwargs...)
    else
        return (more_args...;more_kwargs...) -> flip(f, vcat(args..., more_args...)...;merge(kwargs, more_kwargs)...)
    end
end

function interleave(deepseq::Vector{Vector{T}})::Vector{T} where {T}
    max_len = maximum(length.(deepseq))
    deepseq_padded::Vector{Vector{Union{Nothing, T}}} = Base.map(arr -> vcat(arr..., fill(nothing, max_len - length(arr))), deepseq)
    return vcat(Base.map(filter((!)∘isnothing), eachrow(hcat(deepseq_padded...)))...)
end

function interpose(el::T, seq::Vector{Union{E}})::Vector{Union{T, E}} where {T, E}
    return reduce((acc, item) -> vcat(acc..., item, el), seq, init=Vector{Union{T, E}}())
end

function isuniq(seqs::Vector{T})::Bool where {T}
    return length(unique(seqs)) == length(seqs)
end

function isuniq(str::AbstractString)::Bool
    return length(unique(split(str, ""))) == length(str)
end

# inner join based on left
function arrjoin(seq_a::Vector{<:Tuple}, seq_b::Vector{<:Tuple}, left_on::Function, right_on::Function; how::Symbol=:inner)
    seq_a_grp = groupby(left_on, seq_a)
    seq_b_grp = groupby(right_on, seq_b)
    result = []

    if how == :inner
        common_keys = intersect(Set(keys(seq_a_grp)), Set(keys(seq_b_grp)))
        for k in common_keys
            a_items = get(seq_a_grp, k, [])
            b_items = get(seq_b_grp, k, [])
            for as in a_items, bs in b_items
                push!(result, (as, bs))
            end
        end
    elseif how == :left
        for k in keys(seq_a_grp)
            a_items = get(seq_a_grp, k, [])
            b_items = get(seq_b_grp, k, nothing)
            for as in a_items
                if b_items === nothing
                    push!(result, ((as, (missing, missing))))
                else
                    for bs in b_items
                        push!(result, ((as, bs)))
                    end
                end
            end
        end
    elseif how == :right
        for k in keys(seq_b_grp)
            a_items = get(seq_a_grp, k, nothing)
            b_items = get(seq_b_grp, k, [])
            for bs in b_items
                if a_items === nothing
                    push!(result, (((missing, missing), bs)))
                else
                    for as in a_items
                        push!(result, ((as, bs)))
                    end
                end
            end
        end
    else
        throw(DomainError("$(string(how)) is not defined in arrjoin function. Choose between [:inner, :left, :right]"))
    end

    return result
end
end


