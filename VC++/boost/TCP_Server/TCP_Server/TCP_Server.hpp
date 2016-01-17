#pragma once

#include "Acceptor.hpp"
#include "Connection.hpp"
#include "Workers.hpp"
#include <boost/asio.hpp>

using namespace std;
using namespace boost::asio;

class TCP_Server
{
public:
	TCP_Server() :
		_work(_io),
		_connectionList(_io),
		_acceptor(_io, _connectionList)
	{
	}

	void start() 
	{
		_acceptor.OnConnected = boost::bind(&TCP_Server::on_connected, this, _1);
		_acceptor.wait_for_connection();
		_io.run();
	}
private:
	io_service _io;
	io_service::work _work;
	ConnectionList _connectionList;
	Acceptor _acceptor;
	Workers _workers;
private:
	void on_connected(Connection *connection) {
		connection->wait_for_data();
	}
};