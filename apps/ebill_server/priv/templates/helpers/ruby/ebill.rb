require 'json'

module EBill
  VERSION="0.0.1"

  class <<self
    def info(message) 
      ErlPort::Erlang.call(:error_logger, :info_msg, [message])
    end
    def error(message) 
      ErlPort::Erlang.call(:error_logger, :error_msg, [message])
    end
    def warning(message) 
      ErlPort::Erlang.call(:error_logger, :warning_msg, [message])
    end

    def to_json(data)
      begin
        {:ok => JSON.parse(data)}
      rescue JSON::ParserError => e
        {:error => "JSON Error: #{e}"}
      rescue => e
        {:error => "Internal error: #{e}"}
      end
    end

    def ok(data)
      JSON.generate({:ok => data})
    end

    def error(data)
      JSON.generate({:error => data})
    end
  end
end
