# Simple wireless car key protocol

The wireless key protocol defines an interaction between two participants: a
car and a wireless key.  The protocol begins with the car sending a particular message 
to key, and the key checks if the message matches a particular value.  If matched, the key 
will send back a wake-up signal to the car. The car will then send a challenge to the key, 
which the key must solve and send back the answer to the car.  If the answer is correct, the 
car will unlock, otherwise it will remain locked.

## Running the example

```bash
# in shell 1
cabal run carkey key

# in shell 2
cabal run carkey car

# shell 1 will prompt for the status of car
> Car present

# shell 2 will prompt for the status of key
> Key present

# shell 1 will prompt a quiz from shell 2
# enter the correct answer
> Solve the challenge: 1+1 = ?
> Enter the answer: 
2

# shell 2 will prompt that the car is unlocked
> Unlock the car
```
