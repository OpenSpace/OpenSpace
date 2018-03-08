import React, { Component } from 'react';
import { connect } from 'react-redux';
import PropTypes from 'prop-types';
import MarkerInfo from './MarkerInfo';
import { traverseTreeWithURI, jsonToLuaTable } from '../../utils/propertyTreeHelpers';
import { startListening, stopListening } from '../../api/Actions';

class Markers extends Component {
  constructor(props) {
    super(props);
    this.state = {
      screenSpaceTest: [],
    };
  }

  componentDidUpdate() {
    if (this.props.ScreenSpaceProperties.length > 0) {
      this.props.ScreenSpaceProperties.forEach((property, i) => {
        const screenSpacePos = jsonToLuaTable(property.Value).split(',');
        if (property.listeners === 0) {
          this.props.StartListening(property.Description.Identifier);
          this.state.screenSpaceTest.push(screenSpacePos);
        }
        if ((this.state.screenSpaceTest[i][0] !== screenSpacePos[0] ||
          this.state.screenSpaceTest[i][1] !== screenSpacePos[1])) {
          this.state.screenSpaceTest[i] = screenSpacePos;
        }
      });
    }
  }

  componentWillUnmount() {
    this.props.ScreenSpaceProperties.forEach((property) => {
      this.props.StopListening(property.Description.Identifier);
    });
  }

  createInfoMarkers() {
    const { ScreenSpaceProperties } = this.props;
    const markerInfo = ScreenSpaceProperties.map((property, i) => {
      if (this.state.screenSpaceTest[i][0] !== '-1') {
        const name = property.Description.Identifier.split('.')[1];
        return (<MarkerInfo
          key={property.Description.Identifier}
          name={name}
          position={this.state.screenSpaceTest[i]}
        />);
      }
    });
    return markerInfo;
  }

  render() {
    return (
      <div className={'Markers'}>
        {this.state.screenSpaceTest.length > 0 && this.createInfoMarkers()}
      </div>
    );
  }
}

const mapStateToProps = (state) => {
  const sceneType = 'Scene';
  let nodes = [];
  const ScreenSpaceProperties = [];

  if (Object.keys(state.propertyTree).length !== 0) {
    const rootNodes = state.propertyTree.subowners.filter(element => element.name === sceneType);
    rootNodes.forEach((node) => {
      nodes = [...nodes, ...node.subowners];
    });
    nodes = nodes.filter(node => node.tag.some(tag => tag.includes('Touch.Interesting')))
      .map(node => Object.assign(node, { key: node.name }));

    nodes.forEach((node) => {
      ScreenSpaceProperties.push(traverseTreeWithURI(state.propertyTree, `Scene.${node.name}.RenderableGlobe.ScreenSpacePosition`));
    });
  }
  return {
    ScreenSpaceProperties,
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
  ScreenSpaceProperties: PropTypes.arrayOf(PropTypes.shape({
    Description: PropTypes.string,
    Value: PropTypes.string,
  })),
  StartListening: PropTypes.func,
  StopListening: PropTypes.func,
};

Markers.defaultProps = {
  ScreenSpaceProperties: [],
  Description: [],
  Value: '',
  StopListening: null,
  StartListening: null,
};

export default Markers;
