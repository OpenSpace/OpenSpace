import React, { Component } from 'react';
import PropTypes from 'prop-types';
import PropertyCollection from './PropertyCollection';
import ToggleContent from '../common/ToggleContent/ToggleContent';

import styles from './SceneGraphNode.scss';

class SceneGraphNode extends Component {
  constructor(props) {
    super(props);
  }

  render() {
    const { name, properties, renderable } = this.props;

    return (
      <ToggleContent title={name}>
        { renderable && (<PropertyCollection name="Renderable" properties={renderable.properties} />) }
      </ToggleContent>
    );
  }
}

SceneGraphNode.propTypes = {
  name: PropTypes.string.isRequired,
  properties: PropTypes.arrayOf(PropTypes.object),
  // eslint-disable-next-line react/forbid-prop-types
  renderable: PropTypes.object,
};

SceneGraphNode.defaultProps = {
  properties: [],
  renderable: {},
};

export default SceneGraphNode;
