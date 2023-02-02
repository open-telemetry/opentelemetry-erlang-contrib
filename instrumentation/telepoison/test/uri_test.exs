defmodule Telepoison.URITest do
  @moduledoc """
  Tests for `Telepoison.URI`
  """

  alias HTTPoison.Request

  use ExUnit.Case

  alias Telepoison.URI, as: UtilsURI

  @base_uri "https://www.test.com"

  describe "infer_route_from_request/1" do
    test "Request URL consisiting of whitespace is inferred as a route of '/'" do
      request = %Request{url: ""}

      result = UtilsURI.infer_route_from_request(request)

      assert result == "/"
    end

    test "Request URL '#{@base_uri}/user/edit/24' is inferred as a route of '/user/:subpath'" do
      url = "#{@base_uri}/user/edit/24"
      request = %Request{url: url}

      result = UtilsURI.infer_route_from_request(request)

      assert result == "/user/:subpath"
    end

    test "Request URL '#{@base_uri}/user/24' is inferred as a route of '/user/:subpath'" do
      url = "#{@base_uri}/user/24"
      request = %Request{url: url}

      result = UtilsURI.infer_route_from_request(request)

      assert result == "/user/:subpath"
    end

    test "Request URL #{@base_uri}/'user' is inferred as route of '/user'" do
      url = "#{@base_uri}/user"
      request = %Request{url: url}

      result = UtilsURI.infer_route_from_request(request)

      assert result == "/user"
    end
  end
end
