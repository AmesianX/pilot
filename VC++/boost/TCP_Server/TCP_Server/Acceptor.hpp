#pragma once

#include "config.h"

#include <iostream>
#include <functional>
#include "Connection.hpp"
#include <boost/bind.hpp>
#include <boost/asio.hpp>

using namespace std;
using namespace boost::asio;

class Acceptor
{
public:
	Acceptor(io_service &io, ConnectionList &list) :
		_acceptor(io, ip::tcp::endpoint(ip::tcp::v4(), PORT_NUMBER)),
		_io(io),
		_connectionList(list)
	{
	}

	void wait_for_connection() 
	{
		Connection *connection = _connectionList.get_connection();
		_acceptor.async_accept(connection->get_socket(), boost::bind(&Acceptor::on_connected, this, connection, boost::asio::placeholders::error));
	}
public:
	function<void(Connection*)> OnConnected;
private:
	ip::tcp::acceptor _acceptor;
	io_service &_io;
	ConnectionList &_connectionList;
private:
	void on_connected(Connection *connection, const boost::system::error_code &error)
	{
		std::cout << "Acceptor.on_connected - " << std::endl;

		if (!error) OnConnected(connection);
		wait_for_connection();
	}
};