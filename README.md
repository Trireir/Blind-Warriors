# Blind Warriors

Text based game where four people enter a dark room blindfolded and have to find the others. To make this easy, they can feel the enemys when they are two squares away, and if they meet, will fight until dead.

## Architecture

This system it's a Leader/Worker to create and admin the game rooms and a Client-Server to interact the players with the system. 

To do this, we use a gen_fsm for the players, a gen_server for the game room and a receive server for the central.

## Execution

Open three diferent terminals. In the first on execute this:

	erl -sname server@localhost
	server:start().

This will be the central server. From here we can use diferent functions:

	server:stop()	      # Stop the server
	server:list_rooms()	  # Print the accesible rooms
	server:list_players() # Print the players in the system
	
In other terminal, execute this:

	erl -sname name1@localhost
	super_player:new(name).
	catch player:enter_room().
	
With this, we create a new player with a supervisor to restart him if something goes wrong, and then, find a room to play. The commands to use are the following:

	player:close()		# Leave the game
	player:leave_room()	# Leave the room and go back to the lobby
	player:move(d)		# Move in one direction. This could be u(p), d(own), l(eft) or r(ight)
	player:state()		# Print the information of the player.
	
In the last terminal, create another player, and start to move each other. There are no turns or waits. The first to find the other will have more probabilities to be the winner.

If you are in the same network, you can change localhost with your local ip and play in diferent computers.
