import React, { Component } from 'react';
import { connect } from 'react-redux';
import PropTypes from 'prop-types';
import MarkerInfo from './MarkerInfo';
import { traverseTreeWithURI, jsonToLuaTable } from '../../../utils/propertyTreeHelpers';
import { startListening, stopListening } from '../../../api/Actions/index';
import { fromStringToArray } from '../../../utils/storyHelpers';
import { infoIconKey, OriginKey, FocusNodesListKey } from '../../../api/keys';

class Markers extends Component {
  // Check if the point [x,y] is outside the circle with the center [centerX,centerY] and radius r
  static outsideCircle(centerX, centerY, r, x, y) {
    const squareDist = ((x - centerX) ** 2) + ((y - centerY) ** 2);
    return (squareDist > r ** 2);
  }

  componentDidUpdate() {
    const {
      focusNodes, screenSpaceProperties, screenVisibilityProperties,
      distFromCamToNodeProperties, screenSpaceRadius,
    } = this.props;

    if (focusNodes.length > 0) {
      focusNodes.forEach((node, i) => {
        if (screenSpaceProperties[i].listeners === 0) {
          this.props.StartListening(screenSpaceProperties[i].Description.Identifier);
        }
        if (screenVisibilityProperties[i].listeners === 0) {
          this.props.StartListening(screenVisibilityProperties[i].Description.Identifier);
        }
        if (distFromCamToNodeProperties[i].listeners === 0) {
          this.props.StartListening(distFromCamToNodeProperties[i].Description.Identifier);
        }
        if (screenSpaceRadius[i].listeners === 0) {
          this.props.StartListening(screenSpaceRadius[i].Description.Identifier);
        }
      });
    }
  }

  componentWillUnmount() {
    if (this.props.focusNodes.length > 0) {
      const {
        focusNodes, screenSpaceProperties, screenVisibilityProperties, distFromCamToNodeProperties,
        screenSpaceRadius,
      } = this.props;

      focusNodes.forEach((node, i) => {
        this.props.StopListening(screenSpaceProperties[i].Description.Identifier);
        this.props.StopListening(screenVisibilityProperties[i].Description.Identifier);
        this.props.StopListening(distFromCamToNodeProperties[i].Description.Identifier);
        this.props.StopListening(screenSpaceRadius[i].Description.Identifier);
      });
    }
  }

  createInfoMarkers() {
    const {
      focusNodes, screenSpaceProperties, screenVisibilityProperties,
      distFromCamToNodeProperties, infoIcons, screenSpaceRadius,
      currentFocusNode, focusNodeName, story,
    } = this.props;

    // Get current focus node, its screen space position and its screen space radius
    const focusNodePos = jsonToLuaTable(currentFocusNode.properties.find(property => property.id === 'ScreenSpacePosition').Value).split(',');
    const focusNodeRadius = Number(currentFocusNode.properties.find(property => property.id === 'ScreenSizeRadius').Value);

    return (focusNodes.map((node, i) => {
      const screenSpacePos = jsonToLuaTable(screenSpaceProperties[i].Value).split(',');
      if (screenVisibilityProperties[i].Value === 'true') {
        let outsideCircle = true;
        // Check if node is behind the focus node or not, show label if not behind focus node
        if (node.identifier !== focusNodeName) {
          outsideCircle = Markers
            .outsideCircle(Number(focusNodePos[0]), Number(focusNodePos[1]),
              focusNodeRadius, Number(screenSpacePos[0]), Number(screenSpacePos[1]));
        }
        // TODO Remove the magic numbers
        const distanceFromCam = (100000000000 / distFromCamToNodeProperties[i].Value);
        const showLabel = (distanceFromCam > 0.04 && outsideCircle);

        const planetRadius = Number(screenSpaceRadius[i].Value);
        let size = planetRadius * 0.1;
        const showInfo = (!node.identifier.includes(story.hideinfoicons) && infoIcons.planets &&
          planetRadius > 25 && outsideCircle);

        if (size >= 3) size = 3;
        if (size <= 1.5) size = 1.5;

        let planetInfo;
        if (infoIcons.planets) {
          planetInfo = infoIcons.planets.find(planet => planet.planet === node.identifier);
        }

        return (<MarkerInfo
          key={screenSpaceProperties[i].Description.Identifier}
          identifier={node.identifier}
          position={screenSpacePos}
          size={size}
          showInfo={showInfo}
          planetInfo={planetInfo}
          showLabel={showLabel}
          planetRadius={planetRadius}
        />);
      }
    })
    );
  }

  render() {
    return (
      <div className={'Markers'}>
        {(this.props.screenSpaceProperties.length > 0 && this.props.currentFocusNode)
          && this.createInfoMarkers()}
      </div>
    );
  }
}

const mapStateToProps = (state) => {
  const sceneType = 'Scene';
  let nodes = [];
  let focusNodes = [];
  const screenSpaceProperties = [];
  const screenVisibilityProperties = [];
  const distFromCamToNodeProperties = [];
  let infoIcons = {};
  const screenSpaceRadius = [];
  let focusNodeName = '';
  let currentFocusNode = {};
  const story = state.storyTree.story;

  if (Object.keys(state.propertyTree).length !== 0) {
    const rootNodes = state.propertyTree.subowners
      .filter(element => element.identifier === sceneType);
    rootNodes.forEach((node) => {
      nodes = [...nodes, ...node.subowners];
    });

    if (story.focusbuttons !== undefined) {
      const focusNodesString = traverseTreeWithURI(state.propertyTree, FocusNodesListKey);
      focusNodes = nodes.filter(node =>
        (fromStringToArray(focusNodesString.Value).includes(node.identifier)));

      focusNodes.forEach((node) => {
        screenSpaceProperties.push(traverseTreeWithURI(state.propertyTree, `Scene.${node.identifier}.ScreenSpacePosition`));
        screenVisibilityProperties.push(traverseTreeWithURI(state.propertyTree, `Scene.${node.identifier}.ScreenVisibility`));
        distFromCamToNodeProperties.push(traverseTreeWithURI(state.propertyTree, `Scene.${node.identifier}.DistanceFromCamToNode`));
        screenSpaceRadius.push(traverseTreeWithURI(state.propertyTree, `Scene.${node.identifier}.ScreenSizeRadius`));
      });

      focusNodeName = traverseTreeWithURI(state.propertyTree, OriginKey).Value;
      currentFocusNode = focusNodes.find(node => node.identifier === focusNodeName);
    }
  }

  if (state.storyTree.info.planets) {
    infoIcons = state.storyTree.info;
  }
  return {
    focusNodes,
    screenSpaceProperties,
    screenVisibilityProperties,
    distFromCamToNodeProperties,
    infoIcons,
    screenSpaceRadius,
    focusNodeName,
    currentFocusNode,
    story,
  };
};

const mapDispatchToProps = dispatch => ({
  StartListening: (URI) => {
    dispatch(startListening(URI));
  },
  StopListening: (URI) => {
    dispatch(stopListening(URI));
  },
});

Markers = connect(
  mapStateToProps,
  mapDispatchToProps,
)(Markers);

Markers.propTypes = {
  screenSpaceProperties: PropTypes.arrayOf(PropTypes.shape({
    Description: PropTypes.string,
    Value: PropTypes.string,
  })),
  distFromCamToNodeProperties: PropTypes.arrayOf(PropTypes.shape({
    Description: PropTypes.string,
    Value: PropTypes.string,
  })),
  screenVisibilityProperties: PropTypes.arrayOf(PropTypes.shape({
    Description: PropTypes.string,
    Value: PropTypes.string,
  })),
  screenSpaceRadius: PropTypes.arrayOf(PropTypes.shape({
    Description: PropTypes.string,
    Value: PropTypes.string,
  })),
  infoIcons: PropTypes.arrayOf(PropTypes.shape({
    planet: PropTypes.string,
    info: PropTypes.string,
  })),
  focusNodes: PropTypes.arrayOf(PropTypes.shape({})),
  currentFocusNode: PropTypes.arrayOf(PropTypes.shape({})),
  story: PropTypes.objectOf(PropTypes.shape({})),
  focusNodeName: PropTypes.string,
  StartListening: PropTypes.func,
  StopListening: PropTypes.func,
};

Markers.defaultProps = {
  focusNodes: [],
  screenSpaceProperties: [],
  distFromCamToNodeProperties: [],
  screenVisibilityProperties: [],
  screenSpaceRadius: [],
  infoIcons: [],
  Description: [],
  currentFocusNode: [],
  story: {},
  Value: '',
  focusNodeName: '',
  StopListening: null,
  StartListening: null,
};

export default Markers;
