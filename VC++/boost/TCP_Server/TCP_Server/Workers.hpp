#pragma once

#include "Connection.hpp"

class Workers
{
public:
	void execute(Connection *connection, void *data, int size) 
	{
		// TODO: do something with data and size.

		connection->wait_for_data();
	}
};
