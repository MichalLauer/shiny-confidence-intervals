# Deploy

Deployment is done fully through Github Actions.

# CI/CD

The Github Actions implemented in this project are definitely not optimal
nor simplistic. I tried to use techniques which I have learned in a 
udemy tutorial.

# TODO

- [ ] Scrollable y-axis for generated samples |--x--|
- [ ] Add new tab with the distribution of generated estimates

## Finished

- [x] When there is a lot of samples, the second graph is not well formatted
on the x axis
- [x] Add X samples at once - implement new for loop???
	- [x] append or create completely new?
	- [x] update graphs
	- [x] Fix info text bug
	- [x] Implement waiter
- [x] Use docker to deploy on GCP
  - [x] Create a functional docker file
  - [x] Update deploy-dev action
  - [x] Push to GCP
