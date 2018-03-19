import React, { Component } from 'react';
import PropTypes from 'prop-types';
import PropertyOwner from './Properties/PropertyOwner';
import ToggleContent from '../common/ToggleContent/ToggleContent';
import Button from '../common/Input/Button/Button';
import Icon from '../common/Icon/Icon';
import DataManager from '../../api/DataManager';
import { OriginKey } from '../../api/keys';

class SceneGraphNode extends Component {
  constructor(props) {
    super(props);

    this.focusOnThis = this.focusOnThis.bind(this);
  }

  focusOnThis() {
    const { identifier } = this.props;
    DataManager.setValue(OriginKey, '"' + identifier + '"');
  }

  render() {
    const { identifier, subowners } = this.props;

    return (
      <ToggleContent title={identifier}>
        <Button onClick={this.focusOnThis}>
          <Icon icon="gps_fixed" /> Focus
        </Button>
        { subowners.map(sub => (
          <PropertyOwner
            key={sub.identifier}
            {...sub}
          />
        )) }
      </ToggleContent>
    );
  }
}

SceneGraphNode.propTypes = {
  identifier: PropTypes.string.isRequired,
  // eslint-disable-next-line react/forbid-prop-types
  subowners: PropTypes.arrayOf(PropTypes.shape({
    identifier: PropTypes.string,
    description: PropTypes.string,
    properties: PropTypes.arrayOf(PropTypes.object),
  })),
};

SceneGraphNode.defaultProps = {
  properties: [],
  subowners: [],
};

export default SceneGraphNode;
