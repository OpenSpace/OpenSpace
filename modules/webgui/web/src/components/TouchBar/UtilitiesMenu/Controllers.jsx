import React, { Component } from 'react';
import { connect } from 'react-redux';
import PropTypes from 'prop-types';

import { OriginKey, SetGoToGeoScript, ValuePlaceholder } from '../../../api/keys';
import { traverseTreeWithURI } from '../../../utils/propertyTreeHelpers';
import { changePropertyValue } from '../../../api/Actions';
import DateController from './DateController';
import TimeController from './TimeController';
import SightsController from './SightsController';
import DataManager from '../../../api/DataManager';

class Controllers extends Component {
  constructor(props) {
    super(props);

    this.onChangeSight = this.onChangeSight.bind(this);
  }

  onChangeSight(selected) {
    // Check if the sight is on the current focus, otherwise change focus node
    if (this.props.originNode !== selected.planet) {
      this.props.ChangePropertyValue(this.props.originNode.Description, selected.planet);
    }
    const script = SetGoToGeoScript.replace(ValuePlaceholder, `${selected.location.latitude}, ${selected.location.longitude}, ${selected.location.attitude}`);
    DataManager.runScript(script);
  }

  render() {
    const { story } = this.props;
    return (
      <div style={{ display: 'flex' }}>
        { (story && story.timecontroller !== 'false') &&
          <TimeController />
        }
        {(story && story.datecontroller !== 'false') &&
        <DateController
          dateList={story.datecontroller}
          onChangeSight={this.onChangeSight}
        />}
        {(story && story.sighscontroller !== 'false') &&
        <SightsController
          sightsList={story.sighscontroller}
          onChangeSight={this.onChangeSight}
        />}

      </div>
    );
  }
}

const mapStateToProps = (state) => {
  let originNode = [];
  let nodes = [];
  const sceneType = 'Scene';
  const story = state.storyTree.story;

  if (Object.keys(state.propertyTree).length !== 0) {
    const rootNodes = state.propertyTree.subowners
      .filter(element => element.identifier === sceneType);
    rootNodes.forEach((node) => {
      nodes = [...nodes, ...node.subowners];
    });
    originNode = traverseTreeWithURI(state.propertyTree, OriginKey);
  }


  return {
    originNode,
    story,
  };
};

const mapDispatchToProps = dispatch => ({
  ChangePropertyValue: (description, value) => {
    dispatch(changePropertyValue(description, value));
  },
});

Controllers = connect(
  mapStateToProps,
  mapDispatchToProps,
)(Controllers);

Controllers.propTypes = {
  originNode: PropTypes.arrayOf(PropTypes.shape({
    id: PropTypes.string,
    Description: PropTypes.string,
    Value: PropTypes.string,
    listeners: PropTypes.number,
  })),
  ChangePropertyValue: PropTypes.func,
};

Controllers.defaultProps = {
  originNode: [],
  ChangePropertyValue: () => {},
};

export default Controllers;
