#pragma once

#include <boost/bind.hpp>
#include <boost/asio.hpp>

using namespace boost::asio;

class Connection
{
public:
	Connection(io_service& io) :
		_socket(io)
	{

	}

	void wait_for_data()
	{
		_socket.async_read_some(
			boost::asio::buffer(_buffer),
			boost::bind(&Connection::on_received, this, boost::asio::placeholders::error, boost::asio::placeholders::bytes_transferred));
	}
public:
	ip::tcp::socket &get_socket()
	{
		return _socket;
	}
private:
	ip::tcp::socket _socket;
	std::array<char, 1024*2> _buffer;
private:
	void on_received(const boost::system::error_code& error, size_t size)
	{
		if (error)
		{
			if (error == boost::asio::error::eof)
			{
				std::cout << "Connection.on_received - 클라이언트와 연결이 끊어졌습니다" << std::endl;
			}
			else
			{
				std::cout << "Connection.on_received - error No: " << error.value() << " error Message: " << error.message() << std::endl;
			}

			// TODO: on_disconnected

			return;
		}

		std::cout << "Connection.on_received - size: " << size << std::endl;

		// TODO: event

		wait_for_data();
	}
};

class ConnectionList
{
public:
	ConnectionList(io_service &io) :
		_io(io)
	{

	}

	Connection *get_connection() 
	{
		// TODO: make connection pool

		return new Connection(_io);
	}
private:
	io_service &_io;
};