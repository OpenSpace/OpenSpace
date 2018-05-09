import React, { Component } from 'react';
import { withRouter } from 'react-router-dom';
import { connect } from 'react-redux';
import PropTypes from 'prop-types';
import '../styles/base.scss';

import Error from '../components/common/Error/Error';
import Overlay from '../components/common/Overlay/Overlay';
import {
  changePropertyValue, startConnection, fetchData, addStoryTree, startListening,
} from '../api/Actions';
import TouchBar from '../components/TouchBar/TouchBar';
import styles from './OnTouchGui.scss';
import { traverseTreeWithURI } from '../utils/propertyTreeHelpers';
import {
  infoIconKey, SetGoToGeoScript, ValuePlaceholder, OriginKey, StoryIdentifierKey,
  ApplyRemoveTagKey, ApplyAddTagKey, FocusNodesListKey, SetTimeScript, defaultStory,
} from '../api/keys';
import DataManager from '../api/DataManager';

class OnTouchGui extends Component {
  constructor(props) {
    super(props);

    this.changeStory = this.changeStory.bind(this);
    this.setStory = this.setStory.bind(this);
  }

  componentDidMount() {
    this.props.StartConnection();
    this.props.FetchData(infoIconKey);
  }

  componentDidUpdate() {
    const { storyIdentifierNode, story } = this.props;

    if (storyIdentifierNode.length !== 0) {
      if (storyIdentifierNode.listeners === 0) {
        this.props.StartListening(StoryIdentifierKey);
        this.props.StartListening(FocusNodesListKey);
      }
      if (storyIdentifierNode.Value !== story.storyidentifier) {
        this.addStoryTree(storyIdentifierNode.Value);
      }
    }
  }

  setStory(selectedStory) {
    const {
      storyIdentifierNode, applyRemoveTag, focusNodesList, applyAddTag, focusNode,
    } = this.props;

    // Check if the selected story is different from the OpenSpace property value
    if (storyIdentifierNode.Value !== selectedStory) {
      const json = this.addStoryTree(selectedStory);

      // If the previous story was the default there is no tags to remove
      if (storyIdentifierNode.Value !== defaultStory) {
        this.props.ChangePropertyValue(applyRemoveTag.Description, '');
      }

      // Set all the story specific properties
      this.props.ChangePropertyValue(storyIdentifierNode.Description, selectedStory);
      this.props.ChangePropertyValue(focusNodesList.Description, json.focusbuttons);
      this.props.ChangePropertyValue(applyAddTag.Description, '');
      this.props.ChangePropertyValue(focusNode.Description, json.start.planet);

      const startPosition = json.start.location;
      const goToGeoScript = SetGoToGeoScript.replace(ValuePlaceholder, `${startPosition.latitude}, ${startPosition.longitude}, ${startPosition.attitude}`);
      const setTimeScript = SetTimeScript.replace(ValuePlaceholder, `${json.start.time}`);

      DataManager.runScript(setTimeScript);
      DataManager.runScript(goToGeoScript);
    }
  }

  // Read in json-file for new story and add it to redux
  addStoryTree(selectedStory) {
    const json = require(`../../../../../data/assets/stories/${selectedStory}.json`);
    this.props.AddStoryTree(json);
    return json;
  }

  changeStory(e) {
    this.setStory(e.target.id);
  }

  render() {
    return (
      <div className={styles.app}>
        <button onClick={this.changeStory} id={defaultStory}>Default Story</button>
        <button onClick={this.changeStory} id={'story_solarsystem'}>Solar System Story</button>
        <button onClick={this.changeStory} id={'story_example'}>Example Story</button>

        { this.props.connectionLost && (
          <Overlay>
            <Error>
              Connection lost. Trying to reconnect again soon.
            </Error>
          </Overlay>
        )}
        {(this.props.storyIdentifierNode.length !== 0 &&
          this.props.storyIdentifierNode.Value !== defaultStory) &&
            <TouchBar />
        }
      </div>
    );
  }
}

const mapStateToProps = (state) => {
  let storyIdentifierNode = [];
  let applyRemoveTag = [];
  let applyAddTag = [];
  let focusNodesList = [];
  let focusNode;

  if (Object.keys(state.propertyTree).length !== 0) {
    storyIdentifierNode = traverseTreeWithURI(state.propertyTree, StoryIdentifierKey);
    applyRemoveTag = traverseTreeWithURI(state.propertyTree, ApplyRemoveTagKey);
    applyAddTag = traverseTreeWithURI(state.propertyTree, ApplyAddTagKey);
    focusNodesList = traverseTreeWithURI(state.propertyTree, FocusNodesListKey);
    focusNode = traverseTreeWithURI(state.propertyTree, OriginKey);
  }

  return {
    applyRemoveTag,
    applyAddTag,
    focusNodesList,
    storyIdentifierNode,
    connectionLost: state.connection.connectionLost,
    story: state.storyTree.story,
    focusNode,
  };
};

const mapDispatchToProps = dispatch => ({
  ChangePropertyValue: (discription, URI) => {
    dispatch(changePropertyValue(discription, URI));
  },
  StartConnection: () => {
    dispatch(startConnection());
  },
  FetchData: (id) => {
    dispatch(fetchData(id));
  },
  AddStoryTree: (story) => {
    dispatch(addStoryTree(story));
  },
  StartListening: (URI) => {
    dispatch(startListening(URI));
  },
});

OnTouchGui = withRouter(connect(
  mapStateToProps,
  mapDispatchToProps,
)(OnTouchGui));

OnTouchGui.propTypes = {
  StartConnection: PropTypes.func,
  FetchData: PropTypes.func,
  ChangePropertyValue: PropTypes.func,
  StartListening: PropTypes.func,
  AddStoryTree: PropTypes.func,
  storyIdentifierNode: PropTypes.objectOf(PropTypes.shape({})),
  story: PropTypes.objectOf(PropTypes.shape({})),
  applyRemoveTag: PropTypes.objectOf(PropTypes.shape({})),
  applyAddTag: PropTypes.objectOf(PropTypes.shape({})),
  focusNodesList: PropTypes.objectOf(PropTypes.shape({})),
  focusNode: PropTypes.objectOf(PropTypes.shape({})),
  connectionLost: PropTypes.bool,
};

OnTouchGui.defaultProps = {
  StartConnection: () => {},
  FetchData: () => {},
  ChangePropertyValue: () => {},
  StartListening: () => {},
  AddStoryTree: () => {},
  storyIdentifierNode: {},
  story: {},
  applyRemoveTag: {},
  applyAddTag: {},
  focusNodesList: {},
  focusNode: {},
  connectionLost: null,
};

export default OnTouchGui;
