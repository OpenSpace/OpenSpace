import React from 'react';
import PropTypes from 'prop-types';
import ReactSelect from 'react-select';
import 'react-select/dist/react-select.css';
import Label from '../../common/Label/Label';
import Select from '../../common/Input/Select/Select';
import Row from '../../common/Row/Row';
import styles from './Variables.scss';

const Variables = (props) => {
  const{ variable, onChange } = props;
  const options = 'r theta phi rho T P ur utheta uphi br btheta bphi jr jtheta jphi'
    .split(' ').map(v => ({ value: v, label: v }));

  return(
    <div className={styles.variables}>
      <Label size={'medium'}>Variable: </Label>
      <Select 
        label={variable === '' ? "Select..." : variable}
        options={options}
        onChange={onChange}
        placeholder={''}
      />
    </div>
  );
}


Variables.propTypes = {
  variable: PropTypes.string,
  onChange: PropTypes.func,
};

Variables.defaultProps = {
  variable: '',
  onChange: () => {},
};

export default Variables;