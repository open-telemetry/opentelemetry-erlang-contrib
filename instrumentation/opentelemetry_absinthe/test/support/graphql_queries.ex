defmodule OpentelemetryAbsintheTest.Support.GraphQL.Queries do
  @moduledoc false
  def query do
    """
    query($isbn: String!) {
      book(isbn: $isbn) {
        title
        author {
          name
          age
        }
      }
    }
    """
  end

  def aliased_query do
    """
    query($isbn: String!) {
      alias: book(isbn: $isbn) {
        title
      }
    }
    """
  end

  def empty_query do
    """
    query {
    }
    """
  end

  def mutation do
    """
    mutation($title: String!) {
      create_book(title: $title) {
        title
      }
    }
    """
  end

  def batch_queries do
    """
    query OperationOne($isbn: String!) {
      book(isbn: $isbn) {
        title
      }
    }
    query OperationTwo {
      books {
        pages
      }
    }
    """
  end

  def invalid_query do
    "invalid_query"
  end
end
