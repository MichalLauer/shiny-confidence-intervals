# Deploy

Deployment is done fully through Github Actions.

# CI/CD

The Github Actions implemented in this project are definitely not optimal
nor simplistic. I tried to use techniques which I have learned in a 
udemy tutorial.

# TODO

- [ ] Use docker to deploy on GCP
  - [x] Create a functional docker file
  - [x] Update deploy-dev action
  - [ ] Push to GCP
- [ ] Add ram usage to the bottom
- [ ] Why is there a delay?

## Finished

- [x] When there is a lot of samples, the second graph is not well formatted
on the x axis
- [x] Add X samples at once - implement new for loop???
	- [x] append or create completely new?
	- [x] update graphs
	- [x] Fix info text bug
	- [x] Implement waiter
