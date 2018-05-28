import React, { Component } from 'react';
import { connect } from 'react-redux';
import PropTypes from 'prop-types';

import { OriginKey, SetGoToGeoScript, ValuePlaceholder, ScaleKey } from '../../../../api/keys';
import { traverseTreeWithURI } from '../../../../utils/propertyTreeHelpers';
import { changePropertyValue, startListening, stopListening } from '../../../../api/Actions';
import DateController from './../presentational/DateController';
import TimePlayerController from './../presentational/TimePlayerController';
import SightsController from './../presentational/SightsController';
import ScaleController from './../presentational/ScaleController';
import ToggleBoolButton from './../presentational/ToggleBoolButton';
import DataManager from '../../../../api/DataManager';
import { UpdateDeltaTimeNow } from '../../../../utils/timeHelpers';

class Controllers extends Component {
  constructor(props) {
    super(props);

    this.onChangeSight = this.onChangeSight.bind(this);
    this.onChangeScale = this.onChangeScale.bind(this);
    this.onToggleBoolProperty = this.onToggleBoolProperty.bind(this);
  }

  componentWillReceiveProps() {
    if (this.props.scaleNodes.length !== 0) {
      if (this.props.scaleNodes[0].listeners <= 0) {
        this.props.scaleNodes.forEach(scaleNode =>
          this.props.StartListening(scaleNode.Description.Identifier),
        );
      }
    }
    if (this.props.toggleBoolNodes.length !== 0) {
      if (this.props.toggleBoolNodes[0].listeners <= 0) {
        this.props.toggleBoolNodes.forEach(toggleBoolNode =>
          this.props.StartListening(toggleBoolNode.Description.Identifier),
        );
      }
    }
  }

  componentWillUnmount() {
    if (this.props.scaleNodes.length !== 0) {
      this.props.scaleNodes.forEach(scaleNode =>
        this.props.StopListening(scaleNode.Description.Identifier),
      );
    }
    if (this.props.toggleBoolNodes.length !== 0) {
      this.props.toggleBoolNodes.forEach(toggleBoolNode =>
        this.props.StopListening(toggleBoolNode.Description.Identifier),
      );
    }
  }

  onChangeSight(selected) {
    UpdateDeltaTimeNow(1);
    // Check if the sight is on the current focus, otherwise change focus node
    if (this.props.originNode !== selected.planet) {
      this.props.ChangePropertyValue(this.props.originNode.Description, selected.planet);
    }
    const script = SetGoToGeoScript.replace(ValuePlaceholder, `${selected.location.latitude}, ${selected.location.longitude}, ${selected.location.altitude}`);
    DataManager.runScript(script);
  }

  onChangeScale() {
    const scale = this.props.story.scaleplanets.scale;

    this.props.story.scaleplanets.planets.forEach((planet, i) => {
      if (Number(scale) !== Number(this.props.scaleNodes[i].Value)) {
        this.props.ChangePropertyValue(this.props.scaleNodes[i].Description, scale);
      } else {
        this.props.ChangePropertyValue(this.props.scaleNodes[i].Description, '1');
      }
    });
  }

  onToggleBoolProperty(uri) {
    const propertyNode = this.props.toggleBoolNodes
      .find(property => property.Description.Identifier === uri);
    const value = (propertyNode.Value === 'true') ? '0' : '1';
    this.props.ChangePropertyValue(propertyNode.Description, value);
  }

  render() {
    const { story } = this.props;

    return (
      <div style={{ display: 'flex' }}>
        { (story && story.timecontroller !== 'false') &&
          <TimePlayerController />
        }
        {(story && story.datecontroller !== 'false') &&
        <DateController
          dateList={story.datecontroller}
          onChangeSight={this.onChangeSight}
        />}
        {(story && story.sightscontroller !== 'false') &&
        <SightsController
          sightsList={story.sightscontroller}
          onChangeSight={this.onChangeSight}
        />}
        {(story && story.scaleplanets) &&
          <ScaleController
            info={story.scaleplanets.info}
            scale={(Number(this.props.scaleNodes[0].Value) !== Number(story.scaleplanets.scale))
              ? '1' : story.scaleplanets.scale}
            onChangeScale={this.onChangeScale}
          />
        }
        {(story && story.toggleboolproperties) &&
        <ToggleBoolButton
          properties={story.toggleboolproperties}
          nodes={this.props.toggleBoolNodes}
          onToggle={this.onToggleBoolProperty}
        />
        }
      </div>
    );
  }
}

const mapStateToProps = (state) => {
  let originNode = [];
  let nodes = [];
  const sceneType = 'Scene';
  const story = state.storyTree.story;
  const scaleNodes = [];
  const toggleBoolNodes = [];

  if (Object.keys(state.propertyTree).length !== 0) {
    const rootNodes = state.propertyTree.subowners
      .filter(element => element.identifier === sceneType);
    rootNodes.forEach((node) => {
      nodes = [...nodes, ...node.subowners];
    });

    originNode = traverseTreeWithURI(state.propertyTree, OriginKey);

    if (story.scaleplanets) {
      story.scaleplanets.planets.forEach((node) => {
        scaleNodes.push(traverseTreeWithURI(state.propertyTree, ScaleKey.replace(ValuePlaceholder, `${node}`)));
      },
      );
    }

    if (story.toggleboolproperties) {
      story.toggleboolproperties.forEach((property) => {
        toggleBoolNodes.push(traverseTreeWithURI(state.propertyTree, property.URI));
      },
      );
    }
  }
  return {
    originNode,
    story,
    scaleNodes,
    toggleBoolNodes,
  };
};

const mapDispatchToProps = dispatch => ({
  ChangePropertyValue: (description, value) => {
    dispatch(changePropertyValue(description, value));
  },
  StartListening: (URI) => {
    dispatch(startListening(URI));
  },
  StopListening: (URI) => {
    dispatch(stopListening(URI));
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
  StartListening: PropTypes.func,
  StopListening: PropTypes.func,
  scaleNodes: PropTypes.objectOf(PropTypes.shape({
    Value: PropTypes.string,
    Description: PropTypes.string,
  })),
  toggleBoolNodes: PropTypes.objectOf(PropTypes.shape({
    Value: PropTypes.string,
    Description: PropTypes.string,
  })),
  story: PropTypes.objectOf(PropTypes.shape({})),
};

Controllers.defaultProps = {
  originNode: [],
  scaleNodes: {},
  toggleBoolNodes: {},
  story: {},
  ChangePropertyValue: () => {},
  StartListening: () => {},
  StopListening: () => {},
};

export default Controllers;
