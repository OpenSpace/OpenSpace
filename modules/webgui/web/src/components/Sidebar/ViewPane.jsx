import React, { Component } from 'react';
import PropTypes from 'prop-types';
import Pane from './Pane';
import LoadingString from '../common/LoadingString/LoadingString';
import DataManager from '../../api/DataManager';
import FilterList from '../common/FilterList/FilterList';
import SceneGraphNode from './SceneGraphNode';

const NODES_KEY = '__allNodes';

class ViewPane extends Component {
  constructor(props) {
    super(props);
    this.state = { nodes: [], hasData: false };

    this.receiveData = this.receiveData.bind(this);
  }

  componentDidMount() {
    // subscribe to data
    DataManager.getValue(NODES_KEY, this.receiveData);
  }

  receiveData(data) {
    this.setState({ nodes: data, hasData: true });
  }

  get nodes() {
    return this.state.nodes
      .map(node => Object.assign({ key: node.name }, node));
  }

  render() {
    const { nodes } = this;
    return (
      <Pane title="View" closeCallback={this.props.closeCallback}>
        { !this.state.hasData && (
          <LoadingString loading>
            Loading...
          </LoadingString>
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

export default ViewPane;
