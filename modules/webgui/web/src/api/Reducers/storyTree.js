import { actionTypes } from '../Actions/actionTypes';

const initStoryTree = { story: { storyidentifier: 'story_default' }, reset: false, info: {} };

export const storyTree = (state = initStoryTree, action) => {
  switch (action.type) {
    case actionTypes.addStoryTree:
      return {
        ...state,
        reset: false,
        story: action.payload.story,
      };
    case actionTypes.resetStoryTree:
      return {
        ...state,
        reset: action.payload.reset,
      };
    case actionTypes.addStoryInfo:
      return {
        ...state,
        info: action.payload.info,
      };
    case actionTypes.resetStoryInfo:
      return {
        ...state,
        info: {},
      };
    default:
      return {
        ...state,
      };
  }
};
