import React from 'react';
import PropTypes from 'prop-types';
import 'react-select/dist/react-select.css';
import Label from '../../common/Label/Label';
import Select from '../../common/Input/Select/Select';
import Row from '../../common/Row/Row';
import styles from './Variables.scss';
import Description from '../../common/Description/Description';

const Variables = (props) => {
  const { variable, onChange, disabled, options } = props;

  let processedOptions = []
  for (const opt of options) {
    processedOptions.push({
      label: opt,
      value: opt
    })
  }

  const standardOptions = 'r theta phi rho T P ur utheta uphi br btheta bphi jr jtheta jphi'
    .split(' ').map(v => ({ value: v, label: v }));

  return (
    <div className={styles.variables}>
      <Label size={'medium'}>Variable</Label>
      <Row>
        <Description>Select variable to visualize</Description>
      </Row>
      <Select
        label={variable === '' ? "Select..." : variable}
        options={processedOptions || standardOptions}
        disabled={disabled}
        onChange={onChange}
        placeholder={''}
      />
    </div>
  );
}

Variables.propTypes = {
  variable: PropTypes.string,
  options: PropTypes.arrayOf(PropTypes.string),
  onChange: PropTypes.func,
};

Variables.defaultProps = {
  variable: '',
  options: [],
  onChange: () => { },
};

export default Variables;