import React, { Component } from 'react';
import PropTypes from 'prop-types';
import { excludeKeys } from '../../../../utils/helpers';

import styles from './ScaleInput.scss';

class ScaleInput extends Component {
  constructor(props) {
    super(props);

    this.state = { value: props.defaultValue };

    this.onChange = this.onChange.bind(this);
    this.reset = this.reset.bind(this);
  }

  onChange(event) {
    const { value } = event.currentTarget;
    this.setState({ value });
    this.props.onChange(value);
  }

  get markers() {
    const { leftTicks, rightTicks, centerMarker, defaultValue, min, max } = this.props;
    const showMarkers = centerMarker || (leftTicks > 0 && rightTicks > 0);

    // eslint-disable-next-line no-mixed-operators
    const width = 100 * (defaultValue - min) / (max - min);
    return showMarkers && (
      <div className={styles.markers}>
        <div className={styles.ticks} style={{ width: `calc(${width}% - 2px)` }}>
          { Array.from(new Array(leftTicks), (_, i) =>
            (<div key={i} className={styles.tick} />))}
        </div>
        { centerMarker && (<div className={styles.centerMarker} />) }
        <div className={styles.ticks} style={{ width: `calc(${100 - width}% - 2px)` }}>
          { Array.from(new Array(rightTicks), (_, i) =>
            (<div key={i} className={styles.tick} />))}
        </div>
      </div>
    );
  }

  reset() {
    this.setState({ value: this.props.defaultValue });
    this.props.onChange(this.props.defaultValue);
  }

  render() {
    const { leftLabel, rightLabel, label, wide } = this.props;
    const inheritedProps = excludeKeys(this.props,
      'rightLabel leftLabel label onChange wide centerMarker defaultValue leftTicks rightTicks');

    return (
      <div className={`${styles.group} ${wide ? styles.wide : ''}`}>
        { this.markers}

        <input
          {...inheritedProps}
          value={this.state.value}
          onChange={this.onChange}
          onMouseUp={this.reset}
          onBlur={this.reset}
          type="range"
          className={`${styles.input} ${wide ? styles.wide : ''}`}
        />
        <div className={styles.labels}>
          <span className={styles.leftLabel}>{ leftLabel }</span>
          <label htmlFor={this.props.id}>{ label }</label>
          <span className={styles.rightLabel}>{ rightLabel }</span>
        </div>
      </div>
    );
  }
}

ScaleInput.idCounter = 0;

ScaleInput.propTypes = {
  centerMarker: PropTypes.bool,
  defaultValue: PropTypes.number,
  id: PropTypes.string,
  label: PropTypes.string,
  leftLabel: PropTypes.string,
  leftTicks: PropTypes.number,
  min: PropTypes.number,
  max: PropTypes.number,
  onChange: PropTypes.func,
  rightLabel: PropTypes.string,
  rightTicks: PropTypes.number,
  step: PropTypes.number,
  wide: PropTypes.bool,
};

ScaleInput.defaultProps = {
  centerMarker: true,
  defaultValue: 0,
  id: `scale-${ScaleInput.idCounter++}`,
  label: '',
  leftLabel: '-',
  leftTicks: 5,
  min: -1,
  max: 1,
  onChange: () => {},
  rightLabel: '+',
  rightTicks: 5,
  step: 0.01,
  wide: true,
};

export default ScaleInput;
