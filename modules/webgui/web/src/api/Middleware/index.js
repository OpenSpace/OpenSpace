import { applyMiddleware } from 'redux'
import updateBackend from './sceneGraph'
import logger from './logger'
//Add more reducers here

const middleware = applyMiddleware(
  //logger, //middleWare for logging state change
  updateBackend,
);

export default middleware;