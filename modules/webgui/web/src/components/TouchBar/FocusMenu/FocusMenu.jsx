import React, { Component } from 'react';
import { connect } from 'react-redux';
import PropTypes from 'prop-types';

import DataManager from '../../../api/DataManager';
import FocusButton from './FocusButton';
import { OriginKey } from '../../../api/keys';

// Tag needed for touch nodes
const REQUIRED_TAG = 'Touch.Interesting';

class FocusMenu extends Component {
  constructor(props) {
    super(props);

    this.state = {
      origin: '',
      hasOrigin: false,
    };

    this.updateOrigin = this.updateOrigin.bind(this);
  }

  componentDidMount() {
    DataManager.subscribe(OriginKey, this.updateOrigin);
  }

  componentWillUnmount() {
    DataManager.unsubscribe(OriginKey, this.updateOrigin);
  }

  updateOrigin(data) {
    const { Value } = data;
    this.setState({ origin: Value, hasOrigin: Value !== '' });
  }

  createFocusPickers() {
    const { nodes } = this.props;
    const focusPicker = nodes
      .map(node =>
        (<FocusButton
          key={node.name}
          name={node.name}
          active={this.state.origin}
        />));
    return (focusPicker);
  }

  render() {
    return (
      <div>
        {this.props.nodes.length > 0 && this.createFocusPickers()}
      </div>
    );
  }
}

const mapStateToProps = (state) => {
  const sceneType = 'Scene';
  let nodes = [];
  if (Object.keys(state.propertyTree).length !== 0) {
    const rootNodes = state.propertyTree.subowners.filter(element => element.name === sceneType);
    rootNodes.forEach((node) => {
      nodes = [...nodes, ...node.subowners];
    });
    nodes = nodes.filter(node => node.tag.some(tag => tag.includes(REQUIRED_TAG)))
      .map(node => Object.assign(node, { key: node.name }));
  }
  return {
    nodes,
  };
};

FocusMenu = connect(
  mapStateToProps,
)(FocusMenu);

FocusMenu.propTypes = {
  nodes: PropTypes.arrayOf(PropTypes.shape({
    name: PropTypes.string,
    description: PropTypes.string,
    properties: PropTypes.arrayOf(PropTypes.object),
    subowners: PropTypes.arrayOf(PropTypes.object),
  })),
};

FocusMenu.defaultProps = {
  nodes: [],
  properties: [],
  subowners: [],
};

export default FocusMenu;
