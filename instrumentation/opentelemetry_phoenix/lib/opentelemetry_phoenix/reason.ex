defmodule OpentelemetryPhoenix.Reason do
  def normalize(%{reason: reason}) do
    # %Plug.Conn.WrapperError{}
    normalize(reason)
  end

  def normalize(:badarg) do
    [reason: :badarg]
  end

  def normalize(:badarith) do
    [reason: :badarith]
  end

  def normalize(:system_limit) do
    [reason: :system_limit]
  end

  def normalize(:cond_clause) do
    [reason: :cond_clause]
  end

  def normalize(:undef) do
    [reason: :undef]
  end

  def normalize({:badarity, {fun, args}}) do
    {:arity, arity} = Function.info(fun, :arity)
    [reason: :badarity, function: "#{inspect(fun)}", arity: arity, args: args]
  end

  def normalize({:badfun, term}) do
    [reason: :badfun, term: term]
  end

  def normalize({:badstruct, struct, term}) do
    [reason: :badstruct, struct: struct, term: term]
  end

  def normalize({:badmatch, term}) do
    [reason: :badmatch, term: term]
  end

  def normalize({:badmap, term}) do
    [reason: :badmap, term: term]
  end

  def normalize({:badbool, op, term}) do
    [reason: :badbool, operator: op, term: term]
  end

  def normalize({:badkey, key}) do
    [reason: :badkey, key: key]
  end

  def normalize({:badkey, key, map}) do
    [reason: :badkey, key: key, map: map]
  end

  def normalize({:case_clause, term}) do
    [reason: :case_clause, term: term]
  end

  def normalize({:with_clause, term}) do
    [reason: :with_clause, term: term]
  end

  def normalize({:try_clause, term}) do
    [reason: :try_clause, term: term]
  end

  def normalize({:badarg, payload}) do
    [reason: :badarg, payload: payload]
  end

  def normalize(other) do
    [reason: other]
  end

  def normalize(other, _stacktrace) do
    [reason: other]
  end
end
