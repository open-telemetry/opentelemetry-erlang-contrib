defmodule OpentelemetryAbsintheTest.Support.GraphQL.Schema do
  @moduledoc false

  use Absinthe.Schema

  @books [
    %{isbn: "A1", title: "Fire", pages: 100, author: %{name: "Ale Ali", age: 18}},
    %{isbn: "B2", title: "Water", pages: 200, author: %{name: "Bea Boa", age: 28}},
    %{isbn: "C3", title: "Earth", pages: 300, author: %{name: "Cal Col", age: 38}},
    %{isbn: "D4", title: "Air", pages: 400, author: %{name: "Dan Don", age: 48}}
  ]

  def get_book_by_isbn(isbn) do
    Enum.find(@books, &(Map.fetch!(&1, :isbn) == isbn))
  end

  object :author do
    field(:name, :string)
    field(:age, :integer)
  end

  object :book do
    field(:isbn, :string)
    field(:title, :string)
    field(:pages, :integer)
    field(:author, :author)
  end

  query do
    field :book, :book do
      arg(:isbn, non_null(:string))

      resolve(fn _parent, args, _resolution ->
        {:ok, get_book_by_isbn(args.isbn)}
      end)
    end

    field :books, list_of(:book)
  end

  mutation do
    # stub doesn't actually mutate
    field :create_book, type: :book do
      arg :title, non_null(:string)

      resolve fn _, _, _ -> {:ok, get_book_by_isbn("A1")} end
    end
  end
end
