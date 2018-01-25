import React, { Component } from 'react';
import { connect } from 'react-redux';
import PropTypes from 'prop-types';
import Pane from './Pane';
import FilterList from '../common/FilterList/FilterList';
import SceneGraphNode from './SceneGraphNode';
import LoadingBlocks from '../common/LoadingBlock/LoadingBlocks';

class ViewPane extends Component {
  constructor(props) {
    super(props);
  }

  render() {
    const { nodes } = this.props;
    return (
      <Pane title="View" closeCallback={this.props.closeCallback}>
        { (nodes.length === 0) && (
          <LoadingBlocks className={Pane.styles.loading} />
        )}

        { nodes.length > 0 && (
          <FilterList data={nodes} viewComponent={SceneGraphNode} />
        )}
      </Pane>
    );
  }
}

ViewPane.propTypes = {
  closeCallback: PropTypes.func,
};

ViewPane.defaultProps = {
  closeCallback: null,
};


const mapStateToProps = (state) => {
  const sceneType = 'Scene';
  const rootNodes = state.propertyTree.filter(element => element.name === sceneType);
  let nodes = [];
  rootNodes.forEach((node) => {
    nodes = [...nodes, ...node.subowners];
  });
  return {
    nodes,
  };
};

ViewPane = connect(
  mapStateToProps,
)(ViewPane);

export default ViewPane;
