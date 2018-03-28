import React, { Component } from 'react';
import { connect } from 'react-redux';
import PropTypes from 'prop-types';

import FocusButton from './FocusButton';
import {OriginKey, StoryKey} from '../../../api/keys';
import { changePropertyValue, startListening, stopListening } from '../../../api/Actions';
import { traverseTreeWithURI } from '../../../utils/propertyTreeHelpers';
import styles from './FocusMenu.scss';
import OverViewButton from './OverViewButton';

class FocusMenu extends Component {
  constructor(props) {
    super(props);

    this.state = {
      origin: '',
      listening: false,
    };
  }

  componentDidUpdate(nextProps, nextState) {
    // If a button is clicked change property value
    if (this.state.listening && nextState.origin !== this.state.origin) {
      this.props.ChangePropertyValue(this.props.originNode.Description, this.state.origin);
    }
    // If changes are made in another gui update state
    if (this.state.listening && nextState.origin !== this.props.originNode.Value) {
      this.setState({ origin: this.props.originNode.Value });
    }
    // Start listening on the origin property
    if (!this.state.listening && this.props.nodes.length > 0) {
      this.props.StartListening(OriginKey);
      this.setState({ origin: this.props.originNode.Value, listening: true });
    }
  }

  componentWillUnmount() {
    this.props.StopListening(OriginKey);
    this.setState({ listening: false });
  }

  createFocusButtons() {
    const { nodes } = this.props;
    const focusPicker = nodes
      .map(node =>
        (<FocusButton
          key={node.identifier}
          identifier={node.identifier}
          active={this.state.origin}
          onChangeOrigin={origin => this.setState({ origin })}
        />));
    return (focusPicker);
  }

  render() {
    return (
      <div className={styles.FocusMenu}>
        {this.props.nodes.length > 0 && <OverViewButton />}
        {this.props.nodes.length > 0 && this.createFocusButtons()}
      </div>
    );
  }
}

const mapStateToProps = (state) => {
  const sceneType = 'Scene';
  let originNode = [];
  let nodes = [];

  if (Object.keys(state.propertyTree).length !== 0) {
    const storyIdentifierNode = traverseTreeWithURI(state.propertyTree, StoryKey);
    const rootNodes = state.propertyTree.subowners.filter(element => element.identifier === sceneType);
    rootNodes.forEach((node) => {
      nodes = [...nodes, ...node.subowners];
    });
    nodes = nodes.filter(node => node.tag.some(tag => tag.includes(storyIdentifierNode.Value)))
      .map(node => Object.assign(node, { key: node.identifier }));
    originNode = traverseTreeWithURI(state.propertyTree, OriginKey);
  }
  return {
    nodes,
    originNode,
  };
};

const mapDispatchToProps = dispatch => ({
  ChangePropertyValue: (discription, URI) => {
    dispatch(changePropertyValue(discription, URI));
  },
  StartListening: (URI) => {
    dispatch(startListening(URI));
  },
  StopListening: (URI) => {
    dispatch(stopListening(URI));
  },
});

FocusMenu = connect(
  mapStateToProps,
  mapDispatchToProps,
)(FocusMenu);

FocusMenu.propTypes = {
  nodes: PropTypes.arrayOf(PropTypes.shape({
    identifier: PropTypes.string,
    Description: PropTypes.string,
    properties: PropTypes.arrayOf(PropTypes.object),
    subowners: PropTypes.arrayOf(PropTypes.object),
  })),
  originNode: PropTypes.arrayOf(PropTypes.shape({
    id: PropTypes.string,
    Description: PropTypes.string,
    Value: PropTypes.string,
    listeners: PropTypes.number,
  })),
  ChangePropertyValue: PropTypes.func,
  StartListening: PropTypes.func,
  StopListening: PropTypes.func,
};

FocusMenu.defaultProps = {
  nodes: [],
  properties: [],
  subowners: [],
  originNode: [],
  Description: '',
  Value: '',
};

export default FocusMenu;
