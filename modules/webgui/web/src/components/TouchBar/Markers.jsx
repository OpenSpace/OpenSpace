import React, { Component } from 'react';
import { connect } from 'react-redux';
import PropTypes from 'prop-types';
import MarkerInfo from './MarkerInfo';
import { traverseTreeWithURI, jsonToLuaTable } from '../../utils/propertyTreeHelpers';
import { startListening, stopListening } from '../../api/Actions';

class Markers extends Component {
  componentDidUpdate() {
    const { nodes, screenSpaceProperties, clipSpaceProperties } = this.props;

    if (nodes.length > 0) {
      nodes.forEach((node, i) => {
        if (screenSpaceProperties[i].listeners === 0) {
          this.props.StartListening(screenSpaceProperties[i].Description.Identifier);
        }
        if (clipSpaceProperties[i].listeners === 0) {
          this.props.StartListening(clipSpaceProperties[i].Description.Identifier);
        }
      });
    }
  }

  componentWillUnmount() {
    const { nodes, screenSpaceProperties, clipSpaceProperties } = this.props;
    nodes.forEach((node, i) => {
      this.props.StopListening(screenSpaceProperties[i].Description.Identifier);
      this.props.StopListening(clipSpaceProperties[i].Description.Identifier);
    });
  }

  createInfoMarkers() {
    const { nodes, screenSpaceProperties, clipSpaceProperties } = this.props;

    const markerInfo = nodes.map((node, i) => {
      const screenSpacePos = jsonToLuaTable(screenSpaceProperties[i].Value).split(',');
      const clipSpaceCoord = jsonToLuaTable(clipSpaceProperties[i].Value).split(',');

      if (screenSpacePos[0] !== '-1') {
        // TODO Check for a good function for the size of the icons based on the
        // clipsSpaceCoord.z (10000000000 / clipSpaceCoord[3]) * 0.1;
        const size = 2;
        return (<MarkerInfo
          key={screenSpaceProperties[i].Description.Identifier}
          name={node.name}
          position={screenSpacePos}
          size={size}
        />);
      }
    });
    return markerInfo;
  }

  render() {
    return (
      <div className={'Markers'}>
        {this.props.screenSpaceProperties.length > 0 && this.createInfoMarkers()}
      </div>
    );
  }
}

const mapStateToProps = (state) => {
  const sceneType = 'Scene';
  let nodes = [];
  const screenSpaceProperties = [];
  const clipSpaceProperties = [];

  if (Object.keys(state.propertyTree).length !== 0) {
    const rootNodes = state.propertyTree.subowners.filter(element => element.name === sceneType);
    rootNodes.forEach((node) => {
      nodes = [...nodes, ...node.subowners];
    });

    nodes = nodes.filter(node => node.tag.some(tag => tag.includes('Touch.Interesting')))
      .map(node => Object.assign(node, { key: node.name }));

    nodes.forEach((node) => {
      screenSpaceProperties.push(traverseTreeWithURI(state.propertyTree, `Scene.${node.name}.RenderableGlobe.ScreenSpacePosition`));
    });

    nodes.forEach((node) => {
      clipSpaceProperties.push(traverseTreeWithURI(state.propertyTree, `Scene.${node.name}.RenderableGlobe.ClipSpaceCoordinates`));
    });
  }
  return {
    nodes,
    screenSpaceProperties,
    clipSpaceProperties,
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
  clipSpaceProperties: PropTypes.arrayOf(PropTypes.shape({
    Description: PropTypes.string,
    Value: PropTypes.string,
  })),
  nodes: PropTypes.arrayOf(PropTypes.shape({})),
  StartListening: PropTypes.func,
  StopListening: PropTypes.func,
};

Markers.defaultProps = {
  nodes: [],
  screenSpaceProperties: [],
  clipSpaceProperties: [],
  Description: [],
  Value: '',
  StopListening: null,
  StartListening: null,
};

export default Markers;
