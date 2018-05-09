import { actionTypes } from '../Actions/actionTypes';

const initStoryTree = { story: { storyidentifier: 'story_default' } };

export const storyTree = (state = initStoryTree, action) => {
  switch (action.type) {
    case actionTypes.addStoryTree:
      return {
        story: action.payload.story,
      };
    default:
      return {
        ...state,
      };
  }
};
