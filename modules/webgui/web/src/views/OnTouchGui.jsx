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
  OverlimitKey, ScaleKey,
} from '../api/keys';
import DataManager from '../api/DataManager';
import Slider from '../components/ImageSlider/Slider';
import { UpdateDeltaTimeNow } from '../utils/timeHelpers';
import { toggleShading, toggleHighResolution, toggleHidePlanet } from '../utils/storyHelpers';


class OnTouchGui extends Component {
  constructor(props) {
    super(props);

    this.changeStory = this.changeStory.bind(this);
    this.setStory = this.setStory.bind(this);
    this.getDeveloperButtons = this.getDeveloperButtons.bind(this);
    this.checkStorySettings = this.checkStorySettings.bind(this);
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
    if (this.props.reset) {
      UpdateDeltaTimeNow(1);
      this.setStory(defaultStory);
    }
  }

  // Buttons for developers to change story
  getDeveloperButtons() {
    return (
      <div style={{ width: '200px', display: 'flex', flexDirection: 'column' }}>
        <button onClick={this.changeStory} id={defaultStory}>Default Story</button>
        <button onClick={this.changeStory} id={'story_solarsystem'}>Solar System Story</button>
        <button onClick={this.changeStory} id={'story_example'}>Example Story</button>
        <button onClick={this.changeStory} id={'story_earthweather'}>Earth Weather Story</button>
        <button onClick={this.changeStory} id={'story_jupitermoons'}>Jupiter Moons Story</button>
        <button onClick={this.changeStory} id={'story_mars'}>Mars Story</button>
      </div>
    );
  }

  setStory(selectedStory) {
    const {
      storyIdentifierNode, applyRemoveTag, focusNodesList, applyAddTag, focusNode, overViewNode,
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
      this.props.ChangePropertyValue(overViewNode.Description, json.overviewlimit);

      // Check settings of the previous story and reset values
      this.checkStorySettings(this.props.story, 'true');
      // Check and set the settings of the current story
      this.checkStorySettings(json, 'false');

      // If the previous story scaled planets -> reset value
      if (this.props.story.scaleplanets) {
        this.props.scaleNodes.forEach((planet) => {
          this.props.ChangePropertyValue(planet.Description, '1');
        });
      }

      const startPosition = json.start.location;
      const goToGeoScript = SetGoToGeoScript.replace(ValuePlaceholder, `${startPosition.latitude}, ${startPosition.longitude}, ${startPosition.altitude}`);
      const setTimeScript = SetTimeScript.replace(ValuePlaceholder, `${json.start.time}`);

      DataManager.runScript(setTimeScript);
      DataManager.runScript(goToGeoScript);
    }
  }

  // Check story settings
  checkStorySettings(story, value) {
    const oppositeValue = (value === 'true') ? 'false' : 'true';
    // Check if the story hide any nodes
    if (story.hideplanets) {
      story.hideplanets.forEach(planet => toggleHidePlanet(planet, value));
    }
    // Check if the story have planets with high resolution data
    if (story.highresplanets) {
      story.highresplanets.forEach(planet => toggleHighResolution(planet, oppositeValue));
    }
    // Check if the story have planets with no shading
    if (story.noshadingplanets) {
      story.noshadingplanets.forEach(planet => toggleShading(planet, value));
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
    // {this.getDeveloperButtons()}
    return (
      <div className={styles.app}>
        { this.props.connectionLost && (
          <Overlay>
            <Error>
              Connection lost. Trying to reconnect again soon.
            </Error>
          </Overlay>
        )}
        {(this.props.storyIdentifierNode.length !== 0 &&
          this.props.storyIdentifierNode.Value !== defaultStory)
          ? <TouchBar /> : <Slider changeStory={this.setStory} />
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
  let overViewNode;
  const scaleNodes = [];
  const story = state.storyTree.story;

  if (Object.keys(state.propertyTree).length !== 0) {
    storyIdentifierNode = traverseTreeWithURI(state.propertyTree, StoryIdentifierKey);
    applyRemoveTag = traverseTreeWithURI(state.propertyTree, ApplyRemoveTagKey);
    applyAddTag = traverseTreeWithURI(state.propertyTree, ApplyAddTagKey);
    focusNodesList = traverseTreeWithURI(state.propertyTree, FocusNodesListKey);
    focusNode = traverseTreeWithURI(state.propertyTree, OriginKey);
    overViewNode = traverseTreeWithURI(state.propertyTree, OverlimitKey);

    if (story.scaleplanets) {
      story.scaleplanets.planets.forEach((node) => {
        scaleNodes.push(traverseTreeWithURI(state.propertyTree, ScaleKey.replace(ValuePlaceholder, `${node}`)));
      });
    }
  }

  return {
    applyRemoveTag,
    applyAddTag,
    focusNodesList,
    storyIdentifierNode,
    connectionLost: state.connection.connectionLost,
    story,
    reset: state.storyTree.reset,
    focusNode,
    overViewNode,
    scaleNodes,
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
  overViewNode: PropTypes.objectOf(PropTypes.shape({})),
  scaleNodes: PropTypes.objectOf(PropTypes.shape({})),
  connectionLost: PropTypes.bool,
  reset: PropTypes.bool,
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
  overViewNode: {},
  scaleNodes: {},
  connectionLost: null,
  reset: null,
};

export default OnTouchGui;
