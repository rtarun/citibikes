
bike[,.N]
colnames(bike)
bike[, .(count=count(tripduration)),keyby=.('start station id', 'end station id')]
bike[, .(count=count(tripduration)),by=.('start station id', 'end station id')]
bike[, .(count=count(tripduration)),by=.('start station id')]
bike[, .(count=count(tripduration)),by=.(`start station id`)]
bike[, .(count=.N), by=.(`start station id`)]
bike[, .(count=.N), by=.(`start station id`, `end station id`)]
trips = bike[, .(count=.N), by=.(`start station id`, `end station id`)]
trips = bike[, .(count=.N), by=.(`start station id`, `end station id`)][,order(count)]
trips = bike[, .(count=.N), by=.(`start station id`, `end station id`)]
View(trips)
bike[`start station id`==2006,`start station name`]
bike[`start station id`==3143,unique(`start station name`)]
bike[`start station id`==514,unique(`start station name`)]
bike[`start station id`==426,unique(`start station name`)]
trips = bike[, .(count=.N, sum(subscriber), sum(customer)), by=.(`start station id`, `end station id`)]
trips = bike[, .(count=.N, subscriber=sum(subscriber), customer=sum(customer)), by=.(`start station id`, `end station id`)]
View(trips)
bike[`start station id`==519,unique(`start station name`)]
bike[`start station id`==492,unique(`start station name`)]
bike[`start station id`==477,unique(`start station name`)]
bike[`start station id`==387,unique(`start station name`)]
bb = bike[`start station id`==387 & `end station id`==387, duration]
bb = bike[`start station id`==387 & `end station id`==387, tripduration]
bb
summary(bb)
class(bb)
bb = as.integer(bb)
summary(bb)
bb = as.integer(bb/60)
bb
summary(bb)
