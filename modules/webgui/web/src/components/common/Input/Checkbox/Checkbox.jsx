import React, { Component } from 'react';
import PropTypes from 'prop-types';
import { excludeKeys } from '../../../../utils/helpers';
import styles from './Checkbox.scss';

class Checkbox extends Component {
  constructor(props) {
    super(props);

    this.state = { checked: props.checked || props.value };

    this.onChange = this.onChange.bind(this);
  }

  componentWillReceiveProps({ checked, value }) {
    this.setState({ checked: checked || value });
  }

  onChange(event) {
    const { checked } = event.currentTarget;
    this.setState({ checked });

    this.props.onChange(checked);
  }

  render() {
    const inheritProps = excludeKeys(this.props, 'wide id label onChange type left');
    const { id, label, wide, left } = this.props;
    const { checked } = this.state;

    return (
      <div className={`${styles.wrapper} ${wide ? styles.wide : ''} ${left ? styles.left : ''}`}>
        <input
          {...inheritProps}
          id={id}
          type="checkbox"
          checked={checked}
          onChange={this.onChange}
        />
        <label htmlFor={id}>{ label }</label>
      </div>
    );
  }
}

Checkbox.id = 0;

Checkbox.propTypes = {
  checked: PropTypes.bool,
  id: PropTypes.string,
  /**
   * callback accepting a
   */
  onChange: PropTypes.func,
  label: PropTypes.string.isRequired,
  left: PropTypes.bool,
  value: PropTypes.bool,
  wide: PropTypes.bool,
};

Checkbox.defaultProps = {
  checked: false,
  id: `boolinput-${Checkbox.id++}`,
  left: false,
  onChange: () => {},
  value: false,
  wide: true,
};

export default Checkbox;
