import React, { Component } from 'react';
import PropTypes from 'prop-types';
import styles from './UtilitiesButtons.scss';
import SmallLabel from '../../common/SmallLabel/SmallLabel';

class ToggleBoolButton extends Component {
  constructor(props) {
    super(props);

    this.toggleProperty = this.toggleProperty.bind(this);
  }

  get propertiesButtons() {
    return this.props.properties.map((property, i) => {
      const active = this.props.nodes[i].Value;
      return (
        <div
          className={`${styles.UtilitiesButton} ${active === 'true' && styles.active}`}
          role="button"
          tabIndex="0"
          key={property.URI}
          onClick={this.toggleProperty}
          id={property.URI}
        >
          <SmallLabel id={property.URI} style={{ textAlign: 'center' }}>
            {property.label}
          </SmallLabel>
        </div>
      );
    },
    );
  }

  toggleProperty(e) {
    this.props.onToggle(e.target.id);
  }

  render() {
    return (
      <div style={{ display: 'flex' }}>
        {this.propertiesButtons}
      </div>
    );
  }
}

ToggleBoolButton.propTypes = {
  properties: PropTypes.objectOf(PropTypes.shape({})),
  nodes: PropTypes.objectOf(PropTypes.shape({
    Value: PropTypes.string,
    Description: PropTypes.string,
  })),
  onToggle: PropTypes.func,
};

ToggleBoolButton.defaultProps = {
  properties: {},
  nodes: {},
  onToggle: () => {},
};

export default ToggleBoolButton;
