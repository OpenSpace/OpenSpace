import React, { Component } from 'react';
import { connect } from 'react-redux';
import PropTypes from 'prop-types';

import FocusButton from './FocusButton';
import { OriginKey, ApplyOverviewKey, FocusNodesListKey, ApplyFlyToKey } from '../../../api/keys';
import { changePropertyValue, startListening, stopListening } from '../../../api/Actions';
import { traverseTreeWithURI } from '../../../utils/propertyTreeHelpers';
import styles from './FocusMenu.scss';
import OverViewButton from './OverViewButton';
import {throttle} from "lodash/function";
import { fromStringToArray } from '../../../utils/storyHelpers';

const UpdateDelayMs = 1000;

class FocusMenu extends Component {
  constructor(props) {
    super(props);

    this.state = {
      origin: '',
    };

    this.triggerChanges = throttle(this.triggerChanges.bind(this), UpdateDelayMs);
  }

  componentDidUpdate() {
    if (this.props.originNode.length !== 0) {
      if (this.props.originNode.listeners <= 0) {
        this.props.StartListening(OriginKey);
        this.setState({ origin: this.props.originNode.Value });
      }
      if (this.state.origin !== this.props.originNode.Value) {
        this.setState({ origin: this.props.originNode.Value });
      }
    }
  }

  componentWillUnmount() {
    this.props.StopListening(OriginKey);
  }

  onChangeOrigin(origin) {
    this.props.ChangePropertyValue(this.props.originNode.Description, origin.origin);
  }

  createFocusButtons() {
    const { nodes } = this.props;
    const focusPicker = nodes
      .map(node =>
        (<FocusButton
          key={node.identifier}
          identifier={node.identifier}
          active={this.state.origin}
          onChangeOrigin={origin => this.onChangeOrigin({ origin })}
          onRefocus={this.triggerChanges}
          descriptionFlyTo={this.props.applyFlyTo.Description}
        />));
    return (focusPicker);
  }

  triggerChanges(description){
    this.props.ChangePropertyValue(description,'');
  }

  render() {
    return (
      <div className={styles.FocusMenu}>
        {this.props.nodes.length> 0 &&
          <OverViewButton
            onApplyOverview={this.triggerChanges}
            descriptionOverview={this.props.applyOverview.Description}
          />}
        {this.props.nodes.length > 0 && this.createFocusButtons()}
      </div>
    );
  }
}

const mapStateToProps = (state) => {
  const sceneType = 'Scene';
  let originNode = [];
  let nodes = [];
  let applyOverview;
  let applyFlyTo;

  if (Object.keys(state.propertyTree).length !== 0) {
    const rootNodes = state.propertyTree.subowners.filter(element => element.identifier === sceneType);
    rootNodes.forEach((node) => {
      nodes = [...nodes, ...node.subowners];
    });

    const focusNodesString = traverseTreeWithURI(state.propertyTree, FocusNodesListKey);
    nodes = nodes.filter(node =>
      (fromStringToArray(focusNodesString.Value).includes(node.identifier)));

    originNode = traverseTreeWithURI(state.propertyTree, OriginKey);
    applyOverview = traverseTreeWithURI(state.propertyTree, ApplyOverviewKey);
    applyFlyTo = traverseTreeWithURI(state.propertyTree, ApplyFlyToKey);
  }
  return {
    nodes,
    originNode,
    applyOverview,
    applyFlyTo,
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
  overview: PropTypes.objectOf(PropTypes.shape({
    Identifier: PropTypes.string,
    Description: PropTypes.string,
    Value: PropTypes.string,
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
  overview: {},
  Description: '',
  Value: '',
  ChangePropertyValue: null,
  StopListening: null,
  StartListening: null,
};

export default FocusMenu;
